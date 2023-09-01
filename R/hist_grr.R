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
  histogram <- data |>
    dplyr::group_by(dplyr::across(c(dplyr::all_of(attribs), D_i))) |>
    dplyr::count() |>
    dplyr::ungroup()

  D <- histogram$D_i
  
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
  optin <- data[data$opt_in == TRUE, ]
  truth <- data[data$opt_in == FALSE, ]
  
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
  optin_lie <- optin[flips == "lie", ]

  # sample function
  sample_other_id <- function(D_i, D) {
    
    D_minus_i <- D[D != D_i]
    
    D_i_new <- sample(x = D_minus_i, size = 1)
    
    return(D_i_new)
    
  }
  
  # randomize responses for liars
  optin_lie_perturbed <- optin_lie |>
    dplyr::mutate(D_i = map_chr(.x = D_i, .f = sample_other_id, D = D))
 
  # combine truths and lies
  optin_noisy <- dplyr::bind_rows(optin_truth, optin_lie_perturbed)

  # aggregate -------------------------------------------------------------
  
  # make correction for randomised response
  optin_corrected <- optin_noisy |>
    dplyr::count(D_i, name = "n_v") |>
    dplyr::mutate(n_perturbed = (n_v - N_optin * q) / (p - q)) |>
    dplyr::select(-n_v)
  
  # summarize truth observations
  truth_sum <- dplyr::count(truth, D_i, name = "n_truth")

  # join optin and truth
  combine <- optin_corrected |>
    dplyr::full_join(truth_sum, by = "D_i") |>
    tidyr::replace_na(list(n_perturbed = 0, n_truth = 0)) |>
    dplyr::mutate(n_noisy = n_perturbed + n_truth) |>
    dplyr::select(-n_perturbed, -n_truth)
  
  # join noisy data to data without noise to maintain empty cells and for
  # comparisons
  data_out <- histogram |> 
    dplyr::left_join(combine, by = "D_i") |>
    tidyr::replace_na(list(n = 0, n_noisy = 0)) |>
    dplyr::relocate(D_i, n, n_noisy)
  
  # temporary workaround to keep state identifier
  if(("state" %in% colnames(data)) & (length(unique(data$state)) == 1)) {
    data_out$state <- state_id
  }
  
  return(data_out)

}
