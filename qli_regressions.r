data <- read.csv("./data/merged.csv")

library(dplyr)
library(ggplot2)
library(ggfortify)
library(MASS)

colnames(data)

outdir <- "routput/qli_regression"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
setwd(outdir)

sink("r.log", split = TRUE)

# |------------------------|
# | Regression on the QLIs |
# |------------------------|

attach(data)
data$did_move <- sapply(did_move, function(x) max(x, 0))
data$use_public_transport <- ifelse(
  use_bus == 1 |
    use_metro == 1 |
    use_tram == 1 |
    use_train == 1,
  1, 0
)


qlis <- c(
  "no_study_time", "higher_gpa_if_closer", "no_hobbies", "stress", "no_sleep",
  "no_family", "no_friends", "loneliness"
)

for (qli in qlis) {
  print(qli)

  # Make sure all QLIs are normally distributed
  shapiro_pvalue <- shapiro.test(data[[qli]])$p.value
  if (shapiro_pvalue > 0.05) {
    print("NOT NORMAL")
  }

  full_model <- lm(
    data[[qli]] ~
      commute_time +
      factor(is_commuter) +
      factor(did_move) +
      factor(use_foot) +
      factor(use_bike) +
      factor(use_bus) +
      factor(use_metro) +
      factor(use_tram) +
      factor(use_train) +
      factor(use_car) +
      factor(use_public_transport) +
      n_means_used,
    data = data
  )

  n_coef <- length(coef(full_model))
  n_obs <- nrow(full_model$model)

  aicc <- 2 * n_coef / (n_obs - n_coef - 1)
  result <- stepAIC(full_model, direction = "both", k = aicc, trace = 0)

  print(summary(result))
  print(anova(result))
  pdf(paste(qli, ".pdf", sep = ""))
  print(autoplot(result))
}
