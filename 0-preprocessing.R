## This script applies all preregistered preprocessing steps

library(dplyr)
library(rio)

# load data
dat <- import("raw_data/Rep01_anonymized.csv")
demo <- import("raw_data/Rep01_demographics_anonymized.csv")
nrow(dat)

# delete all-empty rows. As `condition` is defined at the beginning of the data collection, it can be used as filter variable
print(paste0("Number of participants with empty condition: ", sum(dat$condition == "")))
dat <- dat %>% filter(condition != "")

# =================================================================
# From now on, we store deletions in separate Boolean variables and
# do the final selection at the end. This way we can keep track of
# how many participants were deleted for which reason.
# All filter variables are Boolean, start with `has_` and are TRUE
# for participants that should be kept in the analysis.

# Prereg M4: 
# "selection and inclusion/exclusion criteria: All participants are adult (age >= 18)"

# (anything else should be prevented in the survey, but we check anyway)
table(demo$age >= 18)

#------------------------------------------------------
# Prereg AP1: Criteria for post-data collection exclusion of participants, if any
# --> For all analyses, we …
# exclude participants who answered “no” to the question: “I have completed the study conscientiously. I consent to my data being used for the analysis.”

table(dat$conscientious, "no writing assigment" = dat$writing_assignment == "", useNA="ifany")
dat$has_agreed <- is.na(dat$conscientious) | dat$conscientious == 1


# Prereg AP1: "exclude participants who did not write anything in the humility writing exercise."

dat$has_writing_assignment <- dat$writing_assignment != ""

print(paste0("Number of participants with writing assignment: ", sum(dat$has_writing_assignment)))


## Prereg M7: 
# "They excluded participants who “listed a number of strengths and weaknesses that were more than 3 SDs from the mean”. Inspecting the original data file revealed that the procedure was actually “num_strengths_z > 3 OR num_weaknesses_z > 3”

# TODO

# "They excluded participants who “did not follow the directions in the writing section”. Inspecting the original data file revealed that this means “exclude all participants who listed 0 weaknesses”."


# TODO


## Exploratory Analyses

# rename some variables for consistency
dat$has_sound <- dat$sound_on == 1
dat$has_video <- dat$video_played == 1
dat$has_attentiveness <- dat$attentive == 1

table(dat$has_attentiveness, dat$condition)

# TODO: Add more filter variables
dat_final <- dat %>%
  filter(has_agreed, has_writing_assignment)

print(paste0("Number of participants after all exclusions: ", nrow(dat_final)))

export(dat_final, "processed_data/Rep01_cleaned.csv")
