#============================================================
# This file aggegates multiple xlsx files with individual
# strenth/weakness ratings into a single file, computes the ICC of
# the ratings, and creates a consensus rating for each participant.
# Inconsistencies between raters are flagged and need to be resolved manually.
#============================================================

library(rio)
library(dplyr)

#--------------------------------------------------------
# Compute the ICC of strength/weakness ratings

# cobine all xlsx files with strength/weakness ratings
rating_files <- list.files("raw_data/ratings", pattern = "^coding.*\\.xlsx", full.names = TRUE)

ratings <- data.frame()
row_check <- c()

for (i in 1:length(rating_files)) {
#for (i in 1:2) {
  # import the data
  r0 <- import(rating_files[i])
  row_check <- c(row_check, nrow(r0))
  if (i == 1) {
    ratings <- r0
  } else {
    ratings <- merge(ratings, r0, by=c("pid", "writing_assignment"))
  }
  print(nrow(ratings))
}

# plausibility check
if (!all(row_check == nrow(ratings))) {
  stop("Number of rows in rating files does not match!")
}

strength_ratings <- ratings %>% select(starts_with("num_strengths_"))
weak_ratings <- ratings %>% select(starts_with("num_weaks_"))

ICC(strength_ratings)
ICC(weak_ratings)

# Add consensus value column (average), flag inconsistent ratings
ratings$consensus_strengths <- rowMeans(ratings %>% select(starts_with("num_strengths_")), na.rm = TRUE)
ratings$consensus_weaks <- rowMeans(ratings %>% select(starts_with("num_weaks_")), na.rm = TRUE)

ratings$inconsistent_strengths <- apply(ratings %>% select(starts_with("num_strengths_")), 1, function(x) {
  sd(x, na.rm = TRUE) > 0
})  

ratings$inconsistent_weaks <- apply(ratings %>% select(starts_with("num_weaks_")), 1, function(x) {
  sd(x, na.rm = TRUE) > 0
})  

# How many ratings are (in)consistent?

table(ratings$inconsistent_strengths)
table(ratings$inconsistent_weaks)

# Prepare empty columns for the final consensus ratings, which will be filled in manually after discussion of inconsistent
ratings$num_strengths <- NA
ratings$num_weaks <- NA

export(ratings, "export/Rep01_ratings.xlsx")

# manual step: Agree on the consensus rating in the columns num_strengths and num_weaks.
# Save the resulting file as "raw_data/Rep01_ratings.xlsx" and import it in the next step.