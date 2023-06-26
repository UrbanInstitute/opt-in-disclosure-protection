#' Calculate median error
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#'
#' @return Numeric median error
#'
metric_median_error <- function(n, n_noisy) {

  median_error <- stats::median(n_noisy - n)

  return(median_error)

}
