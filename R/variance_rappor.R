#' Calculate variance for RAPPOR method
#'
#' @param epsilon A numeric value of epsilon
#' @param k Cardinality of total values (number of attribute combinations)
#' @param N Number of observations/respondents
#'
#' @return Variance value
#' 
variance_rappor <- function(epsilon, k, N) {
  
  (exp(epsilon / 2)) / (N * (exp(epsilon/2) - 1) ^ 2)
  
}
