#' Calculate root mean square error (RMSE)
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#'
#' @return Numeric root mean square error
#'
metric_rmse <- function(n, n_noisy) {

  rmse <- sqrt(mean((n - n_noisy) ^ 2))

  return(rmse)

}
