#' Calculate variance for S-Hist method
#'
#' @param epsilon A numeric value of epsilon
#' @param k Cardinality of total values (number of attribute combinations)
#' @param N Number of observations/respondents
#'
#' @return Variance value
#' 
variance_shist <- function(epsilon, k, N) {
  
  (exp(epsilon)) / (N * (exp(epsilon) - 1) ^ 2)
  
}
