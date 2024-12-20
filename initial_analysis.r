data <- read.csv("data/merged.csv")
library(ggplot2)
library(dplyr)

n_commuters <- sum(data$is_commuter)
total <- length(data$is_commuter)
n_non_commuters <- total - n_commuters
ratio_commuters <- n_commuters / total

data.frame(
    commuters = n_commuters,
    non_commuters = n_non_commuters,
    ratio_commuters = ratio_commuters
)

attach(data)

boxplot(gpa[gpa != -1] ~ is_commuter[gpa != -1],
    data = data,
    names = c("non commuter", "commuters"),
    xlab = "is_commuter",
    ylab = "gpa"
)

# Histograms for GPA (Commuters)
filtered_commuters <- filter(data, is_commuter == 1 & gpa >= 0 & gpa <= 30)
ggplot(filtered_commuters, aes(x = gpa)) +
  geom_histogram(bins = 15, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..density..), color = "red") + 
  xlim(0, 30) +  
  ggtitle('Histogram of GPA for Commuters') +
  xlab('GPA') +
  ylab('Frequency') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for GPA (Non-commuters)
filtered_non_commuters <- filter(data, is_commuter == 0 & gpa >= 0 & gpa <= 30)
ggplot(filtered_non_commuters, aes(x = gpa)) +
  geom_histogram(bins = 15, fill = "green", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..density..), color = "red") + 
  xlim(0, 30) +  
  ggtitle('Histogram of GPA for Non-commuters') +
  xlab('GPA') +
  ylab('Frequency') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Histogram for mean indices of life quality (Commuters)
filtered_data <- filter(data, is_commuter == 1)
filtered_columns <- filtered_data[, c("no_study_time", "no_hobbies", "stress", "no_sleep", "no_family", "no_friends", "loneliness")]
for (index in colnames(filtered_columns)) {
  xlim_range <- range(filtered_columns[[index]], na.rm = TRUE)
  plot <- ggplot(filtered_columns, aes(x = !!sym(index))) +  
    geom_histogram(bins = 15, alpha = 0.5, fill = "green", color = "black") +
    geom_density(aes(y = ..density..), color = "red") + 
    xlim(xlim_range[1], xlim_range[2]) +  
    ggtitle(paste("Histogram of", index, "for Non-commuters")) +  
    xlab(index) +  
    ylab('Frequency') + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
}

#Histogram for mean indices of life quality (Non-commuters)
filtered_data <- filter(data, is_commuter == 0)
filtered_columns <- filtered_data[, c("no_study_time", "no_hobbies", "stress", "no_sleep", "no_family", "no_friends", "loneliness")]
for (index in colnames(filtered_columns)) {
  xlim_range <- range(filtered_columns[[index]], na.rm = TRUE)
  plot <- ggplot(filtered_columns, aes(x = !!sym(index))) +  
    geom_histogram(bins = 15, alpha = 0.5, fill = "purple", color = "black") +
    geom_density(aes(y = ..density..), color = "red") + 
    xlim(xlim_range[1], xlim_range[2]) + 
    ggtitle(paste("Histogram of", index, "for Non-commuters")) +  
    xlab(index) +  
    ylab('Frequency') + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
}

# Means for life indices
selected_columns <- c("no_study_time", "no_hobbies", "stress", "no_sleep", "no_family", "no_friends", "loneliness")
data$row_mean <- rowMeans(data[, selected_columns], na.rm = TRUE)
result <- data[, c(selected_columns, "row_mean")]
head(result)

#Boxplot of mean life indices for commuters
boxplot(row_mean ~ is_commuter,
        data = data[!is.na(data$row_mean), ],
        names = c("non commuter", "commuters"),
        xlab = "is_commuter",
        ylab = "gpa"
)

#Pie charts
commute_summary <- data.frame(
  group = c("Non-Commuters", "Commuters"),
  value = c(n_non_commuters, n_commuters)
)
commute_summary$fraction <- commute_summary$value / sum(commute_summary$value)
commute_summary$label <- paste0(commute_summary$group, " (", round(commute_summary$fraction * 100), "%)")

ggplot(commute_summary, aes(x = "", y = fraction, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Non-Commuters") +
  theme_void() + 
  theme(legend.position = "right") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

#Commuters
n_10 <- sum(data$commute_time == 0.00 & data$is_commuter==1)
n_30 <- sum(data$commute_time == 0.25 & data$is_commuter==1)
n_1 <- sum(data$commute_time == 0.50 & data$is_commuter==1)
n_1.5 <- sum(data$commute_time == 0.75 & data$is_commuter==1)
n_2 <- sum(data$commute_time == 1.00 & data$is_commuter==1)
commute_time_summary <- data.frame(
  group = c("Less than 10 min.", "10-30 min.", "30 min. to 1 hour", "1 hour to 1.5 hours", "More than 2 hours"),
  value = c(n_10, n_30, n_1, n_1.5,n_2)
)
commute_time_summary$fraction <- commute_time_summary$value / sum(commute_summary$value)
commute_time_summary$label <- paste0(commute_time_summary$group, " (", round(commute_time_summary$fraction * 100), "%)")

ggplot(commute_time_summary, aes(x = "", y = fraction, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percentage of commuting times for Commuters") +
  theme_void() + 
  theme(legend.position = "right") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

#Non-commuters
n_10 <- sum(data$commute_time == 0.00 & data$is_commuter==0)
n_30 <- sum(data$commute_time == 0.25 & data$is_commuter==0)
n_1 <- sum(data$commute_time == 0.50 & data$is_commuter==0)
n_1.5 <- sum(data$commute_time == 0.75 & data$is_commuter==0)
n_2 <- sum(data$commute_time == 1.00 & data$is_commuter==0)
commute_time_summary <- data.frame(
  group = c("Less than 10 min.", "10-30 min.", "30 min. to 1 hour", "1 hour to 1.5 hours", "More than 2 hours"),
  value = c(n_10, n_30, n_1, n_1.5,n_2)
)
commute_time_summary$fraction <- commute_time_summary$value / sum(commute_summary$value)
commute_time_summary$label <- paste0(commute_time_summary$group, " (", round(commute_time_summary$fraction * 100), "%)")

ggplot(commute_time_summary, aes(x = "", y = fraction, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percentage of commuting times for Non-commuters") +
  theme_void() + 
  theme(legend.position = "right") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

#Checking normality of variables: commuters_gpa and noncommuters_gpa
commuters_gpa <- data$gpa[data$is_commuter == 1]
non_commuters_gpa <- data$gpa[data$is_commuter == 0]
shapiro.test(commuters_gpa)
shapiro.test(non_commuters_gpa)

#Permorming Wicoxon test since Shapiro test showed the GPA distribution for commuters was not normal
#Testing the null hypothesis "Commuters have a lower GPA than non-commuters":
wilcox_test_result <- wilcox.test(commuters_gpa, non_commuters_gpa, 
                                  alternative = "less") 
print(wilcox_test_result)

#Anova to test "More time commuting implies lower GPA":
data$commute_time_group <- as.factor(data$commute_time)  
filtered_data <- filter(data, commute_time > 0) 
boxplot(gpa ~ commute_time_group, data = filtered_data,
            xlab = "Commute Time Group",
            ylab = "GPA",
            main = "GPA by Commute Time Group",
            col = c("skyblue", "lightgreen", "pink"))
anova_result <- aov(gpa ~ commute_time_group, data = filtered_data)
summary(anova_result)


