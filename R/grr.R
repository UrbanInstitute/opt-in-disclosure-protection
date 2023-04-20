
grr <- function(data, lookup, epsilon, attrbs) {
  
  # create id
  data <- data %>%
    group_by(across(all_of(attrbs))) %>%
    mutate(id = str_pad(cur_group_id(), width = 3, side = "left", pad = "0"))
  
  lookup <- lookup %>%
    group_by(across(all_of(attrbs))) %>%
    mutate(id = str_pad(cur_group_id(), width = 3, side = "left", pad = "0"))
  
  # set k to number of combinations in lookup table
  k <- nrow(lookup)
  
  # check inputs
  # all ids should be in lookup table
  stopifnot(data$id %in% lookup$id)
  
  # calculate the probability of telling the truth
  p <- exp(epsilon) / (exp(epsilon) + k - 1)
  
  # decide whether to tell the truth or lie
  flips <- sample(
    x = c("truth", "lie"), 
    size = nrow(data), 
    replace = TRUE,
    prob = c(p, (1 - p))
  )

  # keep truth
  # if everything is truth, then return the original data
  data_keep <- data[flips == "truth", ]
  
  if (nrow(data) == nrow(data_keep)) {
    
    return(data)
    
  }
  
  ids_keep <- data_keep$id
  
  # if lie, the pick from another id
  ids_replace <- data$id[flips == "lie"]
  
  new_ids <- map_chr(.x = ids_replace, .f = sample_other_id, ids = lookup$id)
  
  # estimated number of each case, and total N
  N_hat_i <- table(c(new_ids, ids_keep))
  N <- sum(N_hat_i)
  
  # calculate estimated frequency of each class
  # bound estimated proportions between 0 and 1
  f_hat <- function(N_hat) {
    f_hat_i <- ((N_hat / N) - (1 / (exp(epsilon) + k - 1))) * ((exp(epsilon) + k - 1) / (exp(epsilon) - 1))
    f_hat_i_bound <- case_when(f_hat_i > 1 ~ 1,
                               f_hat_i < 0 ~ 0,
                               .default = f_hat_i)
    return(f_hat_i_bound)
  }
  
  f_hat_list <- lapply(N_hat_i, f_hat)
  
  # if any estimated frequency < 0, redistribute to nonzero values
  if (Reduce("+", f_hat_list) < 1){
    remain <- (1 - Reduce("+", f_hat_list)) / sum(f_hat_list != 0)
    f_hat_list <- lapply(f_hat_list[f_hat_list != 0], function(x) x = x + remain)
  }

  # use estimated frequencies to construct noisy data
  # does this have to be a sample or can this be deterministic from estimated proportions?
  synth_ids <- sample(
    x = names(N_hat_i), 
    size = nrow(data), 
    replace = TRUE, 
    prob = f_hat_list
  )
  
  data_synth <- tibble(id = synth_ids) %>%
    left_join(lookup, by = "id")

  return(data_synth)
  
}
