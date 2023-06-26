#' Calculate mean absolute error (MAE)
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#'
#' @return Numeric mean absolute error
#'
metric_mean_abs_error <- function(n, n_noisy) {
  
  mean_abs_error <- mean(abs(n_noisy - n))
  
  return(mean_abs_error)
 
}
