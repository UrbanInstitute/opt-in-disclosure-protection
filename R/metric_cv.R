#' Calculate coefficient of variation (CV)
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#'
#' @return Numeric coefficient of variation
#' 
metric_cv <- function(n, n_noisy) {
  
  source(here::here("R", "metric_rmse.R"))
  
  cv <- metric_rmse(n, n_noisy) / mean(n)
  
  return(cv)
  
}
