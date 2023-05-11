#' Calculate a noisy histogram using generalized randomized response
#'
#' @param data A data frame with micrdodata with attributes and an ID 
#' variable D_i
#' @param epsilon A numeric value of epsilon
#'
#' @return A noisy histogram with n and n_noisy
#' 
hist_grr <- function(data, epsilon) {

  # check inputs ----------------------------------------------------------
  stop_if_not(!is.null(data$D_i))

  # create histogram ------------------------------------------------------

  # all ids should be in lookup table
  histogram <- data %>%
    dplyr::group_by(dplyr::across(dplyr::everything())) %>%
    dplyr::count()

  D <- data %>%
    dplyr::select(-D_i) %>%
    create_D()
  
  N <- nrow(data)
  d <- length(D)
  
  # calculate the probability of telling the truth
  p <- exp(epsilon) / (exp(epsilon) + d - 1)
  q <- 1 / (exp(epsilon) + d - 1)

  # do the probabilities sum to 1?
  stopifnot(round(p + q * (d - 1), 8) == 1)
  
  # encode ----------------------------------------------------------------

  # decide whether to tell the truth or lie
  flips <- sample(
    x = c("truth", "lie"), 
    size = nrow(data), 
    replace = TRUE,
    prob = c(p, q * (d - 1))
  ) 
  
  # keep truth
  # if everything is truth, then return the original data
  data_keep <- data[flips == "truth", ]
  
  if (nrow(data) == nrow(data_keep)) {
    
    return(data)
    
  }
  
  # if lie, the pick from another id
  D_i_replace <- data$D_i[flips == "lie"]

  sample_other_id <- function(D_i, D) {
    
    D_minus_i <- D[D != D_i]
    
    D_i_new <- sample(x = D_minus_i, size = 1)
    
    return(D_i_new)
    
  }
  
  # randomize responses for liars
  D_i_randomized <- map_chr(.x = D_i_replace, .f = sample_other_id, D = D)
 
  # combine truths and lies
  data_noisy <- tibble::tibble(D_i = c(data_keep$D_i, D_i_randomized))
  
  # aggregate -------------------------------------------------------------
  
  # make correction for randomised response
  data_noisy <- count(data_noisy, D_i, name = "n_v") %>%
    dplyr::mutate(n_noisy = (n_v - N * q) / p - q) %>%
    dplyr::select(-n_v)
  
  # join noisy data to data without noise to maintain empty cells and for
  # comparisons
  data_synth <- data_noisy %>% 
    left_join(histogram, by = "D_i") %>%
    dplyr::relocate(D_i, n, n_noisy)

  return(data_synth)

}