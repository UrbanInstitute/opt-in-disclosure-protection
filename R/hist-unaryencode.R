#' Calculate a noisy histogram using unary encoding
#'
#' @param data A data frame with microdata with attributes and ID variable D_i
#' @param epsilon A numeric value of epsilon
#' @param type Symmetric ("SOE") or Optimized ("OUE") string entry
#' @attribs A vector of attributes for the histogram
#'
#' @return A noisy histogram with n and n_noisy
#' 
hist_unaryencode <- function(data, epsilon, type = "SUE", attribs) {

  # check inputs ----------------------------------------------------------
  stopifnot(!is.null(data$D_i))
  stopifnot(!is.null(data$opt_in))
  stopifnot(type %in% c("SUE", "OUE"))

  # temporary workaround to keep state identifier -------------------------
  if(("state" %in% colnames(data)) & (length(unique(data$state)) == 1)) {
    state_id <- unique(data$state)
  }
  
  # create histogram ------------------------------------------------------

  # all ids should be in lookup table
  histogram <- data %>%
    dplyr::group_by(dplyr::across(c(attribs, D_i))) %>%
    dplyr::count()

  D <- data %>%
    dplyr::select(attribs) %>%
    create_D()
  
  N <- nrow(data)
  N_optin <- sum(data$opt_in)
  d <- length(D)

  # calculate p and q from epsilon - for SUE or OUE
  if (type == "SUE"){
    p <- exp(epsilon / 2) / (exp(epsilon / 2) + 1)
    q <- 1 / (exp(epsilon / 2) + 1)
  } else if (type == "OUE"){
    p <- 1/2
    q <- 1 / (exp(epsilon) + 1)
  }
  
  # encode ----------------------------------------------------------------

  # create matrix of OHE responses
  encode_responses <- map(.x = data$D_i, 
                          .f = function(x) as.numeric(x == D))
  response_matrix <- matrix(unlist(encode_responses), 
                            nrow = N, 
                            ncol = d, 
                            byrow = TRUE)

  # split for "optin" and "truth" groups
  optin_matrix <- response_matrix[data$opt_in == TRUE,]
  truth_matrix <- response_matrix[data$opt_in == FALSE,]
  
  # create matrix of random draws for optin group
  sample_matrix <- matrix(runif(nrow(optin_matrix)*d), 
                          nrow = nrow(optin_matrix), 
                          ncol = d, 
                          byrow = TRUE)
  
  # create empty matrix for perturbed bits
  perturb_matrix <- matrix(NA, 
                           nrow = nrow(optin_matrix), 
                           ncol = d, 
                           byrow = TRUE)
  
  # choose to perturb bits based on response, p, and q
  # do not perturb
  perturb_matrix[optin_matrix == 1 & sample_matrix <= p] <- 1
  perturb_matrix[optin_matrix == 0 & sample_matrix > q] <- 0
  # perturb
  perturb_matrix[optin_matrix == 1 & sample_matrix > p] <- 0
  perturb_matrix[optin_matrix == 0 & sample_matrix <= q] <- 1
  
  # aggregate ----------------------------------------------------------------
  
  # calculate column sums and correct for unary encoding
  perturb_sums <- colSums(perturb_matrix)
  perturb_sums_corrected <- map_dbl(.x = perturb_sums, 
                                    .f = function(x) ((x - N_optin*q) / (p-q)))
  
  # calculate truth sums
  truth_sums <- colSums(truth_matrix)
  
  # combine sums, create histogram
  n_noisy <- perturb_sums_corrected + truth_sums
  histogram_noisy <- tibble(D_i = D, n_noisy = n_noisy)
  
  # join histograms
  data_synth <- histogram %>%
    left_join(histogram_noisy, by = "D_i")
  
  # temporary workaround to keep state identifier
  if(("state" %in% colnames(data)) & (length(unique(data$state)) == 1)) {
    data_synth$state <- state_id
  }
  
  return(data_synth)
  
}
