#' Calculate median absolute percent error
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#' @param drop_zeros Logical to drop cells with zeros
#'
#' @return Numeric median absolute percent error
#'
metric_median_abs_pct_error <- function(n, n_noisy, drop_zeros = FALSE) {

  if (drop_zeros) {

    n_noisy <- n_noisy[n != 0]
    n <- n[n != 0]

  }

  median_abs_pct_error <- stats::median(abs(n_noisy - n) / abs(n))

  return(median_abs_pct_error)

}
