#' Calculate median absolute error
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#'
#' @return Numeric median absolute error
#'
metric_median_abs_error <- function(n, n_noisy) {

  median_abs_error <- stats::median(abs(n_noisy - n))

  return(median_abs_error)

}
