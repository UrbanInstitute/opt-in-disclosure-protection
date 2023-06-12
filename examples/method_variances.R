
library(tidyverse)
options(scipen = 999)

source(here::here("R", "variance_all.R"))

epsilon <- .01
k <- 10
N <- 100000

variance_all(epsilon, k, N)
