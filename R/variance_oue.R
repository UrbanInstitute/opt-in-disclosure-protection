#' Calculate variance for OUE method
#'
#' @param epsilon A numeric value of epsilon
#' @param k Cardinality of total values (number of attribute combinations)
#' @param N Number of observations/respondents
#'
#' @return Variance value
#' 
variance_oue <- function(epsilon, k, N) {
  
  return( (4*exp(epsilon)) / (N * (exp(epsilon) - 1)^2) ) 
  
}
