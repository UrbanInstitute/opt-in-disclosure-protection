#' Calculate median percent error
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#' @param drop_zeros Logical to drop cells with zeros
#'
#' @return Numeric median percent error
#'
metric_median_pct_error <- function(n, n_noisy, drop_zeros = FALSE) {

  if (drop_zeros) {
    
    n_noisy <- n_noisy[n != 0]
    n <- n[n != 0]

  }

  median_pct_error <- stats::median((n_noisy - n) / n)

  return(median_pct_error)

}
