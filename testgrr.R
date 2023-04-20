library(tidyverse)

# set.seed(1)
# data <- tibble(x = rbinom(n = 100, size = 1, prob = 0.5)) %>%
#   mutate(id = if_else(x == 0, "0001", "0002"))

data <- tibble(x = c(rep(0, 5000), rep(1, 5000), rep(2, 5000)))  %>%
  mutate(id = case_when(x == 0 ~ "0001", 
                        x == 1 ~ "0002",
                        x == 2 ~ "0003"))

sample_other_id <- function(id, ids) {
  
  ids_disjoint <- ids[ids != id] 
  
  sample(ids_disjoint, size = 1)
  
}



lookup <- tibble(
  x = c(0, 1, 2),
  id = c("0001", "0002", "0003")
)

grr3(data = data, lookup = lookup, epsilon = 0.1)

map_dbl(.x = 1:10, ~mean(grr3(data = data, lookup = lookup, epsilon = 0.1)$data_synth$x == 1))





