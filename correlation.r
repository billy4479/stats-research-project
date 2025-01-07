library(dplyr)
library(corrplot)
library(Hmisc)

data <- read.csv("./data/merged.csv")

outdir <- "routput/correlation"
dir.create(outdir, showWarnings = FALSE)
setwd(outdir)

sink("r.log", split = TRUE)

attach(data)
data$did_move <- sapply(did_move, function(x) max(x, 0))
data$use_public_transport <- ifelse(
  use_bus == 1 |
    use_metro == 1 |
    use_tram == 1 |
    use_train == 1,
  1, 0
)
detach(data)


filtered <- data %>% filter(gpa != -1)
attach(filtered)

selected_columns <- c(
  "commute_time",
  "is_commuter",
  "did_move",
  "no_study_time",
  "higher_gpa_if_closer",
  "no_hobbies",
  "stress",
  "no_sleep",
  "no_family",
  "no_friends",
  "loneliness",
  "attendance",
  "use_foot",
  "use_bike",
  "use_bus",
  "use_metro",
  "use_tram",
  "use_train",
  "use_car",
  "use_public_transport",
  "n_means_used"
)

correlation_matrix <- cor(filtered[selected_columns])
pdf("correlation.pdf", width = 13, height = 13)
corrplot(correlation_matrix,
  method = "number"
  # diag = FALSE
)

rcorr(as.matrix(filtered[selected_columns]))
