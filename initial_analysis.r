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
