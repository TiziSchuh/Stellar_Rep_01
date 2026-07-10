library(rio)
library(dplyr)
library(ggplot2)
library(psych)

dat <- import("processed_data/Rep01_cleaned.csv")

#--------------------------------------------------------
# Create awe scale

awe_items <- dat %>% select(awe, amazement, fascination)

# Three-item awe scale
dat$awe_scale <- rowMeans(awe_items)

# Cronbach's alpha
cronbach_alpha <- alpha(awe_items)
cronbach_alpha

#--------------------------------------------------------
# log-transformation of strength/weakness count

dat$num_strengths_log <- log(dat$num_strengths)
dat$num_weaks_log <- log(dat$num_weaks)

# A better (and established) approach: Add 1 to avoid NaN values
dat$num_strengths1_log <- log(dat$num_strengths + 1)
dat$num_weaks1_log <- log(dat$num_weaks + 1)

dat$balance <- dat$num_strengths / dat$num_weaks
dat$balance_log <- log(dat$balance)

hist(dat$balance_log)

export(dat, "processed_data/Rep01_processed.csv")