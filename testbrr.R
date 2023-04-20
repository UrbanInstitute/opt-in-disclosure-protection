
library(tidyverse)

data <- tibble(x = c(rep(0, 5000), rep(1, 5000)))  %>%
    mutate(id = if_else(x == 0, "0001", "0002"))

sample_other_id <- function(id, ids) {
  
  ids_disjoint <- ids[ids != id]
  sample(ids_disjoint, size = 1)
  
}

lookup <- tibble(
  x = c(0, 1),
  id = c("0001", "0002")
)

brr(data = data, lookup = lookup, epsilon = 0.1)

map_dbl(.x = 1:10, ~mean(brr(data = data, lookup = lookup, epsilon = 0.1)$x == 1))
