#============================================================
# This script applies all preregistered preprocessing steps
#============================================================

library(dplyr)
library(rio)

# load data
dat0 <- import("raw_data/Rep01_anonymized.csv")
demo0 <- import("raw_data/Rep01_demographics_anonymized.csv")
nrow(dat0)
ratings <- import("raw_data/Rep01_ratings.xlsx")

# merge the ratings into the main data frame
dat0 <- left_join(
  dat0, 
  ratings %>% select(pid, num_strengths, num_weaks), 
  by = c("pid"))

# delete all-empty rows. As `condition` is defined at the beginning of the data collection, it can be used as filter variable
print(paste0("Number of participants with empty condition: ", sum(dat0$condition == "")))
dat0 <- dat0 %>% filter(condition != "")

print(paste0("Non-zombie participants: ", nrow(dat0)))

export(dat0, "processed_data/Rep01_no_zombies.csv")

# =================================================================
# Factor levels for demographics

demo <- demo0 %>%
  mutate(
    gender = factor(gender, 
      levels = 1:6, 
      labels = c("female", "male", "non-binary", "no gender", "prefer not to say", "other"))
  )


# =================================================================
# From now on, we store deletions in separate Boolean variables and
# do the final selection at the end. This way we can keep track of
# how many participants were deleted for which reason.
# All filter variables are Boolean and start with `has_`.

# Prereg M4: 
# "selection and inclusion/exclusion criteria: All participants are adult (age >= 18)"

# (anything else should be prevented in the survey, but we check anyway)
table(demo$age >= 18)

#------------------------------------------------------
# Prereg AP1: Criteria for post-data collection exclusion of participants, if any
# --> For all analyses, we …
# exclude participants who answered “no” to the question: “I have completed the study conscientiously. I consent to my data being used for the analysis.”

# For participants who have a missing value in the variable (because they dropped out before the last survey page), we assume their agreement, as they agreed to the general informed consent at the beginning of the survey.

table(dat0$conscientious, "no writing assigment" = dat0$writing_assignment == "", useNA="ifany")
dat0$has_agreed <- is.na(dat0$conscientious) | dat0$conscientious == 1


# Prereg AP1: "exclude participants who did not write anything in the humility writing exercise."

dat0$has_writing_assignment <- dat0$writing_assignment != ""

print(paste0("Number of participants with writing assignment: ", sum(dat0$has_writing_assignment)))


## Prereg M7: 
# "They excluded participants who “listed a number of strengths and weaknesses that were more than 3 SDs from the mean”. Inspecting the original data file revealed that the procedure was actually “num_strengths_z > 3 OR num_weaknesses_z > 3”

dat0$num_strengths_z <- scale(dat0$num_strengths)
dat0$num_weaknesses_z <- scale(dat0$num_weaks)

# Any outliers?
dat0$has_outlier <- abs(dat0$num_strengths_z) > 3 | abs(dat0$num_weaknesses_z) > 3

table(dat0$has_outlier)

# "They excluded participants who “did not follow the directions in the writing section”. Inspecting the original data file revealed that this means “exclude all participants who listed 0 weaknesses”."

table(dat0$num_weaks, useNA="ifany")
dat0$has_zero_weaks <- dat0$num_weaks == 0
table(dat0$has_zero_weaks)

table(has_zero=dat0$has_zero_weaks, has_writing=dat0$has_writing_assignment)


## Exploratory Analyses

# rename some variables for consistency
dat0$has_sound <- dat0$sound_on == 1
dat0$has_video <- dat0$video_played == 1
dat0$has_attentiveness <- dat0$attentive == 1

table(dat0$has_attentiveness, dat0$condition)

# These filters are applied to all subsequent analyses - therefore, we apply them here.
# All other filters are only applied to some analyses, and are applied in the respective analysis scripts.
dat <- dat0 %>%
  filter(has_agreed, has_writing_assignment)

print(paste0("Number of participants after general exclusions: ", nrow(dat)))

table(dat$condition)

# export the pids of the remaining participants for syncing the demographics data
export(dat %>% select(pid), "export/valid_pids.csv")

export(dat, "processed_data/Rep01_cleaned.csv")
export(demo, "processed_data/Rep01_demo_cleaned.csv")