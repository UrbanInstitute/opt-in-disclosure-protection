#' Calculate variance for w-SM method
#'
#' @param epsilon A numeric value of epsilon
#' @param k Cardinality of total values (number of attribute combinations)
#' @param N Number of observations/respondents
#'
#' @return Variance value
#' 
variance_wsm <- function(epsilon, k, N) {
  
  return( (exp(epsilon) + k - 2) / (N * (exp(epsilon) - 1)^2) )
  
}
