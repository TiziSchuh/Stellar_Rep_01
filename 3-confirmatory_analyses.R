library(ggplot2)
library(ggstatsplot)

dat <- import("processed_data/Rep01_processed.csv")

# Prereg I3 Hypothesis:
# ========================================================

# H1: Relative to a neutral control condition, the original awe inducing video increases humility in a behavioral measure.

# Ensure that we apply the preregistered exclusion criteria to the data before testing this hypothesis:
dat_H1 <- dat %>%
  filter(!has_outlier, !has_zero_weaks) %>%
  filter(condition %in% c("exp_old", "control"))

ggbetweenstats(
  data = dat_H1,
  x    = condition,
  y    = balance_log,
  type = "parametric"
)



# H2: Relative to a neutral control condition, the new awe inducing video increases humility in a behavioral measure.
