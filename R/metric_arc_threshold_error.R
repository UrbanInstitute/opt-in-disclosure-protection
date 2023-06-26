#' Calculate arc threshold error
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#' @param threshold Numeric in \[0, 1\] for the percent error threshold
#' @param drop_zeros Logical to drop cells with zeros
#'
#' @return Numeric arc threshold error
#'
metric_arc_threshold_error <- function(n, n_noisy, threshold, drop_zeros = FALSE) {

  if (drop_zeros) {

    n_noisy <- n_noisy[n != 0]
    n <- n[n != 0]

  }

  arc_threshold_error <- mean(arc_percent(n, n_noisy) > threshold)

  return(arc_threshold_error)

}
