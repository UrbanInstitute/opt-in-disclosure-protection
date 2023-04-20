
brr <- function(data, lookup, epsilon) {
  
  # check inputs
  # all ids should be in lookup table
  stopifnot(data$id %in% lookup$id)
  
  # calculate the probability of telling the truth
  p <- exp(epsilon) / (1 + exp(epsilon))
  
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
  N_hat_1 <- N_hat_i[1]
  
  N <- sum(N_hat_i)

  # calculate estimated frequency of class 1
  f_hat_1 <- ((N_hat_1 / N) - (1 / (exp(epsilon) + 1))) * ((exp(epsilon) + 1) / (exp(epsilon) - 1))
  
  # bound estimated proportions between 0 and 1
  f_hat_1_bound <- case_when(f_hat_1 > 1 ~ 1,
                             f_hat_1 < 0 ~ 0,
                             .default = f_hat_1)
  
  # use estimated frequencies to construct noisy data
  # does this have to be a sample or can this be deterministic from estimated proportions?
  synth_ids <- sample(
    x = names(N_hat_i), 
    size = nrow(data), 
    replace = TRUE, 
    prob = c(f_hat_1_bound, (1 - f_hat_1_bound))
  )
  
  data_synth <- tibble(id = synth_ids) %>%
    left_join(lookup, by = "id")
  
  return(data_synth)

}
