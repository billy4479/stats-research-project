data <- read.csv("./data/merged.csv")

library(dplyr)
library(ggplot2)
library(ggfortify)
library(MASS)

outdir <- "rplots"
dir.create(outdir, showWarnings = FALSE)
setwd(outdir)

sink("r.log", split = TRUE)

# |-----------------------|
# | Regression on the GPA |
# |-----------------------|

# Some samples have an invalid gpa
filtered <- data %>% filter(gpa != -1)

commuters <- filtered %>% filter(is_commuter == 1)
non_commuters <- filtered %>% filter(is_commuter == 0)

# Non-commuters are not normal. This is probably due to the lack of data.
shapiro.test(commuters$gpa)
shapiro.test(non_commuters$gpa)

# The combined data is normal at alpha = 0.05
shapiro.test(filtered$gpa)

attach(filtered)

full_model <- lm(
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
summary(full_model)


n_coef <- length(coef(full_model))
n_obs <- nrow(full_model$model)

aicc <- 2 * n_coef / (n_obs - n_coef - 1)
result <- stepAIC(full_model, direction = "both", k = aicc)
summary(result)
anova(result)

pdf("gpa.pdf")
autoplot(result)
