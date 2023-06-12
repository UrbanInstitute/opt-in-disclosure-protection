#' Calculate median absolute arc percent error
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#' @param drop_zeros Logical to drop cells with zeros
#'
#' @return Numeric median absolute arc percent error
#' 
metric_median_abs_arc_pct_error <- function(n, n_noisy, drop_zeros = FALSE) {
  
  if (drop_zeros) {
    
    n_noisy <- n_noisy[n != 0]
    n <- n[n != 0]
    
  }
  
  median_abs_arc_pct_error <- stats::median(abs(arc_percent(n, n_noisy)))
  
  return(median_abs_arc_pct_error)
  
}
