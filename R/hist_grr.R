#' Calculate a noisy histogram using generalized randomized response
#'
#' @param data A data frame with microdata with attributes and an ID 
#' variable D_i
#' @param epsilon A numeric value of epsilon
#' @param attribs A vector of attributes for the histogram

#' @return A noisy histogram with n and n_noisy
#' 
hist_grr <- function(data, epsilon, attribs) {

  # check inputs ----------------------------------------------------------
  stopifnot(!is.null(data$D_i))
  stopifnot(!is.null(data$opt_in))
  
  # temporary workaround to keep state identifier -------------------------
  if(("state" %in% colnames(data)) & (length(unique(data$state)) == 1)) {
    state_id <- unique(data$state)
  }
  
  # create histogram ------------------------------------------------------

  # all ids should be in lookup table
  histogram <- data %>%
    dplyr::group_by(dplyr::across(c(all_of(attribs), D_i))) %>%
    dplyr::count() %>%
    dplyr::ungroup()

  D <- data %>%
    dplyr::select(all_of(attribs)) %>%
    create_D()
  
  N <- nrow(data)
  N_optin <- sum(data$opt_in)
  d <- length(D)
  
  # calculate the probability of telling the truth
  p <- exp(epsilon) / (exp(epsilon) + d - 1)
  q <- 1 / (exp(epsilon) + d - 1)

  # do the probabilities sum to 1?
  stopifnot(round(p + q * (d - 1), 8) == 1)
  
  # encode ----------------------------------------------------------------

  # split for "optin" and "truth" groups
  optin <- data[data$opt_in == TRUE,]
  truth <- data[data$opt_in == FALSE,]
  
  # for opt-ins, decide whether to tell the truth or lie
  flips <- sample(
    x = c("truth", "lie"), 
    size = nrow(optin), 
    replace = TRUE,
    prob = c(p, q * (d - 1))
  )
  
  # for opt-ins, keep truth flip untouched
  optin_truth <- optin[flips == "truth", ]
  
  # for opt-ins, if lie flip, the pick from another id
  optin_lie <- optin[flips == "lie",]

  # sample function
  sample_other_id <- function(D_i, D) {
    
    D_minus_i <- D[D != D_i]
    
    D_i_new <- sample(x = D_minus_i, size = 1)
    
    return(D_i_new)
    
  }
  
  # randomize responses for liars
  optin_lie_perturbed <- optin_lie %>%
    mutate(D_i = map_chr(.x = D_i, .f = sample_other_id, D = D))
 
  # combine truths and lies
  optin_noisy <- rbind(optin_truth, optin_lie_perturbed)

  # aggregate -------------------------------------------------------------
  
  # make correction for randomised response
  optin_corrected <- count(optin_noisy, D_i, name = "n_v") %>%
    dplyr::mutate(n_perturbed = (n_v - N_optin * q) / (p - q)) %>%
    dplyr::select(-n_v)
  
  # summarize truth observations
  truth_sum <- count(truth, D_i, name = "n_truth")

  # join optin and truth
  combine <- optin_corrected %>%
    left_join(truth_sum, by = "D_i") %>%
    mutate(n_noisy = rowSums(select(., "n_perturbed", "n_truth"), na.rm = TRUE)) %>%
    select(-c(n_perturbed, n_truth))
  
  # join noisy data to data without noise to maintain empty cells and for
  # comparisons
  data_out <- combine %>% 
    left_join(histogram, by = "D_i") %>%
    dplyr::relocate(D_i, n, n_noisy)
  
  # temporary workaround to keep state identifier
  if(("state" %in% colnames(data)) & (length(unique(data$state)) == 1)) {
    data_out$state <- state_id
  }
  
  return(data_out)

}
