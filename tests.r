data <- read.csv("data/merged.csv")

library(dplyr)

outdir <- "routput/tests"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
setwd(outdir)

sink("r.log", split = TRUE)

attach(data)

n_commuters <- sum(is_commuter)
total <- length(is_commuter)
n_non_commuters <- total - n_commuters
ratio_commuters <- n_commuters / total

data.frame(
  commuters = n_commuters,
  non_commuters = n_non_commuters,
  ratio_commuters = ratio_commuters
)

selected_columns <- c("no_study_time", "no_hobbies", "stress", "no_sleep", "no_family", "no_friends", "loneliness")
data$row_mean <- rowMeans(data[, selected_columns], na.rm = TRUE)
result <- data[, c(selected_columns, "row_mean")]
head(result)

# Checking normality of variables: commuters_gpa and noncommuters_gpa
commuters_gpa <- data$gpa[data$is_commuter == 1 & data$gpa != -1]
non_commuters_gpa <- data$gpa[data$is_commuter == 0 & data$gpa != -1]
shapiro.test(commuters_gpa)
shapiro.test(non_commuters_gpa)

# Permorming Wilcoxon rank-sum test since Shapiro test showed the GPA distribution for commuters was not normal
# Testing the hypothesis "Commuters have a lower GPA than non-commuters":
wilcox_test_result <- wilcox.test(commuters_gpa, non_commuters_gpa,
  alternative = "less",
  exact = FALSE
)
print(wilcox_test_result)

# Anova to test "More time commuting implies lower GPA":
data$commute_time_group <- as.factor(data$commute_time)
filtered_data <- data %>% filter(commute_time >= 0 & gpa != -1)

anova_result <- aov(gpa ~ commute_time_group, data = filtered_data)
summary(anova_result)

# Testing the hypothesis "Commuters have a lower mean of life indices":
selected_columns <- c("no_study_time", "no_hobbies", "stress", "no_sleep", "no_family", "no_friends", "loneliness")
data$row_mean <- rowMeans(data[, selected_columns], na.rm = TRUE)
commuters_data <- filter(data, is_commuter == 1)
non_commuters_data <- filter(data, is_commuter == 0)

# Shapiro-Wilk test for normality of 'row_mean'
shapiro_commuters <- shapiro.test(commuters_data$row_mean)
shapiro_non_commuters <- shapiro.test(non_commuters_data$row_mean)
shapiro_commuters
shapiro_non_commuters
# t-test as they're both normally distributed
t_test_result <- t.test(commuters_data$row_mean, non_commuters_data$row_mean,
  alternative = "less", var.equal = TRUE
)
t_test_result
