#============================================================
# Diverse exploratory tests
#============================================================

library(ggplot2)
library(ggstatsplot)

dat <- import("processed_data/Rep01_processed.csv")

# Alternative Version of log transformation for H1:
# Use the balance1_log measure (with +1 added before log transformation)
# ========================================================

# H1: Relative to a neutral control condition, the original awe inducing video increases humility in a behavioral measure.

# This time, do *not* exclude the zero-weak cases. Also keep the outliers.
  dat_H1_alt <- dat %>%
    filter(condition %in% c("exp_old", "control"))

ggbetweenstats(
  data = dat_H1_alt,
  x    = condition,
  y    = balance1_log,
  type = "parametric"
)

t.test(
  dat_H1_alt$balance1_log[dat_H1_alt$condition == "control"],
  dat_H1_alt$balance1_log[dat_H1_alt$condition == "exp_old"],
  alternative = "greater",
  var.equal = TRUE
)




## Prereg I4: Exploratory research questions
#---------------------------------------------------------------

# E1: The new (more modern) awe inducing video leads to higher awe ratings compared to the old awe inducing video.


# E2: The new (more modern) awe inducing video leads to more behavioral humility compared to the old awe inducing video.


# E3: The screen size moderates the awe inducing effect of the video (awe video vs. control condition), with larger screens leading to larger effects.


# E4: We will explore alternative ways of operationalizing the behavioral measure which do not discard participants with zero reported weaknesses.


# E5: We will exclude participants based on some post-randomization control questions (being aware of consequences for causal inference due to the breaking of randomization) 


# E6: We explore the moderating effect of participants’ self-reported attentiveness on the awe-inducing effect of the video (awe video vs. control condition).

