#============================================================
# Perform the preregistered confirmatory analyses
#============================================================

library(ggplot2)
library(ggstatsplot)
library(pwr)

dat <- import("processed_data/Rep01_processed.csv")

# Prereg I3 Hypothesis:
# ========================================================

# H1: Relative to a neutral control condition, the original awe inducing video increases humility in a behavioral measure.

# Ensure that we apply the preregistered exclusion criteria to the data before testing this hypothesis:

table(Outlier = dat$has_outlier, Zero_Weaks= dat$has_zero_weaks)
table(dat$condition, Zero_Weaks= dat$has_zero_weaks)
table(dat$has_zero_weaks)

dat_H1 <- dat %>%
  filter(!has_outlier, !has_zero_weaks) %>%
  filter(condition %in% c("exp_old", "control"))

nrow(dat_H1)

ggbetweenstats(
  data = dat_H1,
  x    = condition,
  y    = balance_log,
  type = "parametric"
)

# Store the t.test in an object, so that we can reference it in the manuscript.
t_H1 <- t.test(
  dat_H1$balance_log[dat_H1$condition == "control"],
  dat_H1$balance_log[dat_H1$condition == "exp_old"],
  alternative = "greater",
  var.equal = TRUE
)

t_H1

# Compute the actually achieved power (because we did not achieve the planned sample size at the end of the semester).
# --> We actually achieved 70% power to detect the effect size of d = 0.51 reported in the original publication.
pwr.t.test(
  n = nrow(dat_H1) / 2,
  d = 0.51,
  sig.level = 0.05,
  type = "two.sample",
  alternative = "greater"
)



# H2: Relative to a neutral control condition, the new awe inducing video increases humility in a behavioral measure.

dat_H2 <- dat %>%
  filter(!has_outlier, !has_zero_weaks) %>%
  filter(condition %in% c("exp_new", "control"))

ggbetweenstats(
  data = dat_H2,
  x    = condition,
  y    = balance_log,
  type = "parametric"
)

t_H2 <- t.test(
  dat_H2$balance_log[dat_H2$condition == "control"],
  dat_H2$balance_log[dat_H2$condition == "exp_new"],
  alternative = "greater",
  var.equal = TRUE
)

t_H2
