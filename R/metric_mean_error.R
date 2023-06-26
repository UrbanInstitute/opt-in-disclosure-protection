#' Calculate mean error (ME)
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#'
#' @return Numeric mean error
#'
metric_mean_error <- function(n, n_noisy) {

  mean_error <- mean(n_noisy - n)

  return(mean_error)

}
