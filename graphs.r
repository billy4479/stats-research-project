data <- read.csv("data/merged.csv")
library(ggplot2)
library(dplyr)

outdir <- "routput/graphs"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
setwd(outdir)

sink("r.log", split = TRUE)

attach(data)
n_commuters <- sum(is_commuter)
total <- length(is_commuter)
n_non_commuters <- total - n_commuters
ratio_commuters <- n_commuters / total

# Histograms for GPA (Commuters)
pdf("histogram_gpa_commuters.pdf")
filtered_commuters <- filter(data, is_commuter == 1 & gpa >= 0 & gpa <= 30)
ggplot(filtered_commuters, aes(x = gpa)) +
  geom_histogram(bins = 12, fill = "purple", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(density)), color = "red") +
  xlim(18, 30) +
  ggtitle("Histogram of GPA for Commuters") +
  xlab("GPA") +
  ylab("Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for GPA (Non-commuters)
pdf("histogram_gpa_noncommuters.pdf")
filtered_non_commuters <- filter(data, is_commuter == 0 & gpa >= 0 & gpa <= 30)
ggplot(filtered_non_commuters, aes(x = gpa)) +
  geom_histogram(bins = 12, fill = "green", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(density)), color = "red") +
  xlim(18, 30) +
  ggtitle("Histogram of GPA for Non-commuters") +
  xlab("GPA") +
  ylab("Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot gpa for commuters vs non-commuters
pdf("boxplot_gpa_vs_is_commuter.pdf")
boxplot(gpa[gpa != -1] ~ is_commuter[gpa != -1],
  data = data,
  names = c("non commuter", "commuters"),
  xlab = "is_commuter",
  ylab = "gpa"
)

# Histogram for mean indices of life quality (Commuters)
filtered_data <- filter(data, is_commuter == 1)
filtered_columns <- filtered_data[, c("no_study_time", "no_hobbies", "stress", "no_sleep", "no_family", "no_friends", "loneliness")]
for (index in colnames(filtered_columns)) {
  pdf(paste("histogram_", index, "_commuters.pdf", sep = ""))
  xlim_range <- range(filtered_columns[[index]], na.rm = TRUE)
  plot <- ggplot(filtered_columns, aes(x = !!sym(index))) +
    geom_histogram(bins = 10, alpha = 0.5, fill = "purple", color = "black") +
    geom_density(aes(y = ..density..), color = "red") +
    xlim(xlim_range[1], xlim_range[2]) +
    ggtitle(paste("Histogram of", index, "for Non-commuters")) +
    xlab(index) +
    ylab("Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
}

# Histogram for mean indices of life quality (Non-commuters)
filtered_data <- filter(data, is_commuter == 0)
filtered_columns <- filtered_data[, c("no_study_time", "no_hobbies", "stress", "no_sleep", "no_family", "no_friends", "loneliness")]
for (index in colnames(filtered_columns)) {
  pdf(paste("histogram_", index, "_noncommuters.pdf", sep = ""))
  xlim_range <- range(filtered_columns[[index]], na.rm = TRUE)
  plot <- ggplot(filtered_columns, aes(x = !!sym(index))) +
    geom_histogram(bins = 10, alpha = 0.5, fill = "green", color = "black") +
    geom_density(aes(y = ..density..), color = "red") +
    xlim(xlim_range[1], xlim_range[2]) +
    ggtitle(paste("Histogram of", index, "for Non-commuters")) +
    xlab(index) +
    ylab("Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
}

# Boxplot of mean life indices for commuters
selected_columns <- c("no_study_time", "no_hobbies", "stress", "no_sleep", "no_family", "no_friends", "loneliness")
data$row_mean <- rowMeans(data[, selected_columns], na.rm = TRUE)
result <- data[, c(selected_columns, "row_mean")]

pdf("boxplot_mean_qli_vs_is_commuter.pdf")
boxplot(row_mean ~ is_commuter,
  data = data[!is.na(data$row_mean), ],
  names = c("non commuter", "commuters"),
  xlab = "is_commuter",
  ylab = "gpa"
)

# Pie charts
commute_summary <- data.frame(
  group = c("Non-Commuters", "Commuters"),
  value = c(n_non_commuters, n_commuters)
)
commute_summary$fraction <- commute_summary$value / sum(commute_summary$value)
commute_summary$label <- paste0(commute_summary$group, " (", round(commute_summary$fraction * 100), "%)")

colors <- c("Non-Commuters" = "green", "Commuters" = "purple")

pdf("pie_commuters_vs_noncommuters.pdf")
ggplot(commute_summary, aes(x = "", y = fraction, fill = group)) +
  geom_bar(width = 1, stat = "identity", color = "black", alpha = 0.5) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Non-Commuters and Commuters") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(
    values = colors
  )

# Commuters
n_10_commuters <- sum(data$commute_time == 0.00 & data$is_commuter == 1)
n_30_commuters <- sum(data$commute_time == 0.25 & data$is_commuter == 1)
n_1_commuters <- sum(data$commute_time == 0.50 & data$is_commuter == 1)
n_1.5_commuters <- sum(data$commute_time == 0.75 & data$is_commuter == 1)
n_2_commuters <- sum(data$commute_time == 1.00 & data$is_commuter == 1)

commute_time_summary_commuters <- data.frame(
  group = c("Less than 10 min.", "10-30 min.", "30 min. to 1 hour", "1 hour to 1.5 hours", "More than 2 hours"),
  value = c(n_10_commuters, n_30_commuters, n_1_commuters, n_1.5_commuters, n_2_commuters)
)
commute_time_summary_commuters$fraction <- commute_time_summary_commuters$value / sum(commute_time_summary_commuters$value)
commute_time_summary_commuters$label <- paste0(commute_time_summary_commuters$group, " (", round(commute_time_summary_commuters$fraction * 100), "%)")

purple_shades <- c("#E6E6FA", "#D8BFD8", "#BA55D3", "#8A2BE2", "#4B0082")

# Pie chart for commuters
pdf("pie_commute_time_commuters.pdf")
ggplot(commute_time_summary_commuters, aes(x = "", y = fraction, fill = group)) +
  geom_bar(width = 1, stat = "identity", color = "black", alpha = 0.7) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Commuting Times for Commuters") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(
    values = purple_shades,
    breaks = commute_time_summary_commuters$group,
    labels = commute_time_summary_commuters$label
  )

# Non-commuters
n_10_non_commuters <- sum(data$commute_time == 0.00 & data$is_commuter == 0)
n_30_non_commuters <- sum(data$commute_time == 0.25 & data$is_commuter == 0)
n_1_non_commuters <- sum(data$commute_time == 0.50 & data$is_commuter == 0)
n_1.5_non_commuters <- sum(data$commute_time == 0.75 & data$is_commuter == 0)
n_2_non_commuters <- sum(data$commute_time == 1.00 & data$is_commuter == 0)

commute_time_summary_non_commuters <- data.frame(
  group = c(
    "Less than 10 min.",
    "10-30 min.",
    "30 min. to 1 hour",
    "1 hour to 1.5 hours",
    "More than 2 hours"
  ),
  value = c(
    n_10_non_commuters,
    n_30_non_commuters,
    n_1_non_commuters,
    n_1.5_non_commuters,
    n_2_non_commuters
  )
)
commute_time_summary_non_commuters$fraction <-
  commute_time_summary_non_commuters$value /
    sum(commute_time_summary_non_commuters$value)
commute_time_summary_non_commuters$label <-
  paste0(
    commute_time_summary_non_commuters$group,
    " (", round(commute_time_summary_non_commuters$fraction * 100), "%)"
  )

green_shades <- c("#E6F9E6", "#C2F0C2", "#66CC66", "#009900", "#006400")

# Pie chart for non-commuters
pdf("pie_commute_time_noncommuters.pdf")
ggplot(commute_time_summary_non_commuters, aes(x = "", y = fraction, fill = group)) +
  geom_bar(width = 1, stat = "identity", color = "black", alpha = 0.7) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Commuting Times for Non-commuters") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(
    values = green_shades,
    breaks = commute_time_summary_non_commuters$group,
    labels = commute_time_summary_non_commuters$label
  )


data$commute_time_group <- as.factor(data$commute_time)
filtered_data <- filter(data, commute_time >= 0 & gpa != -1)

pdf("boxplot_gpa_vs_commute_time.pdf")
boxplot(gpa ~ commute_time_group,
  data = filtered_data,
  xlab = "Commute Time Group",
  ylab = "GPA",
  main = "GPA by Commute Time Group",
  col = scales::alpha(
    c("#E6E6FA", "#D8BFD8", "#BA55D3", "#8A2BE2", "#4B0082"),
    alpha = 0.7
  )
)
