#============================================================
# Create the awe scale and log-transform the behavioral measure
#============================================================

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
# original log-transformation of strength/weakness count
# This breaks for all cases with 0 weaknesses

dat$num_strengths_log <- log(dat$num_strengths)
dat$num_weaks_log <- log(dat$num_weaks)

dat$balance <- dat$num_strengths / dat$num_weaks
dat$balance_log <- log(dat$balance)

# A better (and established) approach: Add 1 to avoid NaN values
dat$num_strengths1_log <- log(dat$num_strengths + 1)
dat$num_weaks1_log <- log(dat$num_weaks + 1)

dat$balance1 <- (dat$num_strengths + 1) / (dat$num_weaks + 1)
dat$balance1_log <- log(dat$balance1)

# compare
hist(dat$balance_log)
hist(dat$balance1_log)
hist(dat$balance1)

export(dat, "processed_data/Rep01_processed.csv")