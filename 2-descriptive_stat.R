#============================================================
# Report descriptives stats
#============================================================

library(rio)
library(dplyr)
library(ggplot2)
library(psych)
library(effectsize)

dat <- import("processed_data/Rep01_processed.csv")
demo <- import("processed_data/Rep01_demo_cleaned.csv")

# Demographics
#============================================================

nrow(demo)
summary(demo$age)
sd(demo$age)

table(demo$gender)



# Scales
#============================================================

summary(dat$awe_scale)

# From the original publication:
# "Participants then reported how much happiness and fear as well as awe, wonder, and amazement they felt from 1 (not at all) to 7 (very much), which were made into a composite (alpha= .96). In this study the awe video elicited greater awe (M=5.82, SD=1.18), t(84)=12.18, p<.001, and happiness (M=4.66, SD=1.70), t(86)=5.30, p<.001, than the neutral video (awe ratings: M=2.29, SD=1.49; happy ratings: M=2.77, SD=1.64)."

# --> Why is fear not reported?

dat %>%
  group_by(condition) %>%
  summarise(
    awe_M = mean(awe_scale),
    awe_SD = sd(awe_scale),
    happy_M = mean(happiness),
    happy_SD = sd(happiness),
  )


# Manipulation check: Do the videos elicit different emotions?

ggplot(dat, aes(x=awe_scale, color=condition)) +
  geom_density() +
  theme_minimal() +
  labs(title="Density of Awe Scale by Condition", x="Awe Scale", y="Density")

t1_old <- t.test(
  dat$awe_scale[dat$condition == "control"], 
  dat$awe_scale[dat$condition == "exp_old"]
)

t1_old
effectsize(t1_old)


t1_new <- t.test(
  dat$awe_scale[dat$condition == "control"], 
  dat$awe_scale[dat$condition == "exp_new"]
)

t1_new
effectsize(t1_new)

# Is there are difference in elicited awe between the old and the new video?
t_new_old <- t.test(
  dat$awe_scale[dat$condition == "exp_new"], 
  dat$awe_scale[dat$condition == "exp_old"]
)

t_new_old
effectsize(t_new_old)

