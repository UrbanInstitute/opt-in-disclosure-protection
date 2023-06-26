#' Calculate mean arc percent error
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#' @param drop_zeros Logical to drop cells with zeros
#'
#' @return Numeric mean arc percent error
#' 
metric_mean_arc_pct_error <- function(n, n_noisy, drop_zeros = FALSE) {
  
  if (drop_zeros) {
    
    n_noisy <- n_noisy[n != 0]
    n <- n[n != 0]
  
  }
  
  mean_arc_pct_error <- mean(arc_percent(n, n_noisy))
  
  return(mean_arc_pct_error)
  
}
