data <- read.csv("./data/merged.csv")

library(dplyr)
library(ggplot2)
library(MASS)

source("lib.r")
outdir <- "routput/gpa_regression"
dir.create(outdir, showWarnings = FALSE)
setwd(outdir)

sink("r.log", split = TRUE)

# Some samples have an invalid gpa
filtered <- data %>% filter(gpa != -1)

commuters <- filtered %>% filter(is_commuter == 1)
non_commuters <- filtered %>% filter(is_commuter == 0)

# The combined data is still not normal
test_normality(filtered, "gpa")

attach(filtered)

full_model <- lm(
  gpa ~
    commute_time +
    # factor(is_commuter) +
    # factor(did_move) +
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
result <- stepAIC(full_model, direction = "both", k = aicc, trace = 2)
summary(result)
anova(result)

pdf("scatter_residuals.pdf")
ggplot(result, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "Residual vs. Fitted Values Plot", x = "Fitted Values", y = "Residuals")

test_normality(result, "residuals")
