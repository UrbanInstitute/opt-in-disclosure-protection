#' Calculate variance for omega-SM method
#'
#' @param epsilon A numeric value of epsilon
#' @param k Cardinality of total values (number of attribute combinations)
#' @param N Number of observations/respondents
#'
#' @return Variance value
#' 
variance_omega_sm <- function(epsilon, k, N) {
  
  (exp(epsilon) + k - 2) / (N * (exp(epsilon) - 1) ^ 2)
  
}
