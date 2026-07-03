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


#--------------------------------------------------------
# log-transformation of strength/weakness count


export(dat, "processed_data/Rep01_processed.csv")