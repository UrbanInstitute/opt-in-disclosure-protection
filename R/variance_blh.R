#' Calculate variance for BLH method
#'
#' @param epsilon A numeric value of epsilon
#' @param k Cardinality of total values (number of attribute combinations)
#' @param N Number of observations/respondents
#'
#' @return Variance value
#' 
variance_blh <- function(epsilon, k, N) {
  
  return( ((exp(epsilon) + 1)^2) / (N * (exp(epsilon) - 1)^2) ) 
  
}
