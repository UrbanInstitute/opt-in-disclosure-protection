#' Calculate variance for BRR method
#'
#' @param epsilon A numeric value of epsilon
#' @param k Cardinality of total values (number of attribute combinations)
#' @param N Number of observations/respondents
#'
#' @return Variance value
#' 
variance_brr <- function(epsilon, k, N) {
  
  (exp(epsilon)) / (N * (exp(epsilon) - 1) ^ 2)
  
}
