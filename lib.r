library(ggplot2)

test_normality <- function(data, column_name, name = "") {
  if (name == "") {
    name <- column_name
  }

  print(paste("TEST NORMALITY:", name))
  x <- data[[column_name]]
  sw_test <- shapiro.test(x)

  # Print test results
  cat("Shapiro-Wilk Normality Test Results:\n")
  cat("W statistic:", sw_test$statistic, "\n")
  cat("p-value:", sw_test$p.value, "\n")
  cat("\nInterpretation:\n")
  if (sw_test$p.value < 0.05) {
    cat("The data significantly deviates from normality (p < 0.05)\n")
  } else {
    cat("Failed to reject normality (p >= 0.05)\n")
  }

  df_qq <- data.frame(
    sample = sort(x),
    theoretical = qnorm(ppoints(length(x))),
    # Calculate confidence intervals for the QQ line
    conf_lower = qnorm(ppoints(length(x))) * sd(x) + mean(x) -
      1.96 * sd(x) * sqrt(1 / length(x)),
    conf_upper = qnorm(ppoints(length(x))) * sd(x) + mean(x) +
      1.96 * sd(x) * sqrt(1 / length(x))
  )

  pdf(paste("qqplot_", name, ".pdf", sep = ""))
  plot <- ggplot(df_qq, aes(x = theoretical, y = sample)) +
    # Add confidence interval bands
    geom_ribbon(aes(ymin = conf_lower, ymax = conf_upper),
      fill = "grey80", alpha = 0.5
    ) +
    # Add the theoretical line
    geom_abline(
      intercept = mean(x) - sd(x) * mean(qnorm(ppoints(length(x)))),
      slope = sd(x),
      color = "red",
      linetype = "dashed"
    ) +
    # Add the actual points
    geom_point(alpha = 0.5) +
    # Customize labels and theme
    labs(
      title = paste("Normal Q-Q Plot for", name),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  print(plot)
}
