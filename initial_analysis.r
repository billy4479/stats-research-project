data <- read.csv("data/merged.csv")

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
