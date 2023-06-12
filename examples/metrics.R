
source(here::here("R", "metric_all.R"))

truth <- c(14, 14, 0, 6, 30, 20)
estimate <- c(10, 14, 2, 10, 35, 20)
threshold <- 0.2

metric_all(truth, estimate, 0.2, drop_zeros = TRUE)
