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
  plot <- ggplot(filtered_columns, aes_string(x = index)) +  
    geom_histogram(bins = 15, alpha = 0.5, fill = "green", color = "black") +
    geom_density(aes(y = ..density..), color = "red") + 
    xlim(xlim_range) +  # Set xlim based on data range
    ggtitle(paste("Histogram of", index, "for Commuters")) +  
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
  plot <- ggplot(filtered_columns, aes_string(x = index)) +  
    geom_histogram(bins = 15, alpha = 0.5, fill = "green", color = "black") +
    geom_density(aes(y = ..density..), color = "red") + 
    xlim(xlim_range) +  # Set xlim based on data range
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
