data <- read.csv("./data/merged.csv")

library(dplyr)
library(ggplot2)
library(ggfortify)

colnames(data)

outdir <- "rplots"
dir.create(outdir, showWarnings = FALSE)
setwd(outdir)

sink("r.log", split = TRUE)

# Some samples have an invalid gpa
filtered <- data %>% filter(gpa != -1)

commuters <- filtered %>% filter(is_commuter == 1)
non_commuters <- filtered %>% filter(is_commuter == 0)

# Non-commuters are not normal. This is probably due to the lack of data.
shapiro.test(commuters$gpa)
shapiro.test(non_commuters$gpa)

# The combined data is normal at alpha = 0.05
shapiro.test(filtered$gpa)

# |-----------------------|
# | Regression on the GPA |
# |-----------------------|

attach(filtered)
regression <- lm(
  gpa ~
    commute_time +
    factor(is_commuter) +
    factor(did_move) +
    no_study_time +
    higher_gpa_if_closer +
    no_hobbies +
    stress +
    no_sleep +
    no_family +
    no_friends +
    loneliness +
    # no_hobbies * stress * no_sleep * no_family * no_friends * loneliness +
    attendance,
  data = filtered
)
detach(filtered)

summary(regression)

pdf("gpa.pdf")
autoplot(regression)

# |------------------------|
# | Regression on the QLIs |
# |------------------------|

data$did_move <- sapply(data$did_move, function(x) max(x, 0))

attach(data)

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

  regression <- lm(
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
      n_means_used,
    data = data
  )
  print(summary(regression))
  print(anova(regression))
  pdf(paste(qli, ".pdf", sep = ""))
  print(autoplot(regression))
}
