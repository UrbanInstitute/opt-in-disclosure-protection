brr <- function(data, lookup, epsilon) {
  
  # check inputs
  # all ids should be in lookup table
  
  
  # calculate the probability of telling the truth
  p <- exp(epsilon) / (1 + exp(epsilon))
  
  # decide whether to tell the truth or lie
  flips <- sample(
    x = c("truth", "lie"), 
    size = nrow(data), 
    replace = TRUE,
    prob = c(p, (1 - p))
  ) 
  
  print(mean(flips == "lie"))
  
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
  
  N_hat_i <- table(c(new_ids, ids_keep))
  N_hat_1 <- N_hat_i[1]
  N_hat_2 <- N_hat_i[2]
  
  #k <- length(N_hat_i)
  
  N <- sum(N_hat_i)

  #f_hat_i <- (p - 1) / (2 * p - 1) + (N_hat_i / ((2 * p - 1) * N))
  
  # f_hat_i <- ((N_hat_i / N) - (1 / (exp(epsilon) + k - 1))) *
  #   ((exp(epsilon) + k - 1) / (exp(epsilon) - 1))

  f_hat_1 <- ((N_hat_1 / N) - (1 / (exp(epsilon) + 1))) * ((exp(epsilon) + 1) / (exp(epsilon) - 1))

  if (f_hat_1 > 1){
    f_hat_1 <- 1
  }
  
  if (f_hat_1 < 0){
    f_hat_1 <- 0
  }
  
  synth_ids <- sample(
    x = names(N_hat_i), 
    size = nrow(data), 
    replace = TRUE, 
    prob = c(f_hat_1, (1 - f_hat_1))
  )
  
  data_synth <- tibble(id = synth_ids) %>%
    left_join(lookup, by = "id")
  
  var_f_hat_1 <- exp(epsilon) / (N * (exp(epsilon) - 1)**2)
  
  return_list <- list(data_synth = data_synth, var_f_hat_1 = var_f_hat_1)

  return(return_list)

}