#' Calculate all metrics
#'
#' @param n Vector of true values
#' @param n_noisy Vector of noisy values
#' @param threshold Numeric in \[0, 1\] for the percent error threshold for relevant metrics
#' @param drop_zeros Logical to drop cells with zeros for relevant metrics
#'
#' @return Tibble of metrics and numeric values
#'
metric_all <- function(n, n_noisy, threshold, drop_zeros = FALSE) {

  source(here::here("R", "metric_mean_error.R"))
  source(here::here("R", "metric_mean_abs_error.R"))
  source(here::here("R", "metric_mean_pct_error.R"))
  source(here::here("R", "metric_mean_abs_pct_error.R"))
  source(here::here("R", "metric_median_error.R"))
  source(here::here("R", "metric_median_abs_error.R"))
  source(here::here("R", "metric_median_pct_error.R"))
  source(here::here("R", "metric_median_abs_pct_error.R"))
  source(here::here("R", "metric_rmse.R"))
  source(here::here("R", "metric_cv.R"))
  source(here::here("R", "metric_threshold_error.R"))
  source(here::here("R", "arc_percent.R"))
  source(here::here("R", "metric_mean_arc_pct_error.R"))
  source(here::here("R", "metric_mean_abs_arc_pct_error.R"))
  source(here::here("R", "metric_median_arc_pct_error.R"))
  source(here::here("R", "metric_median_abs_arc_pct_error.R"))
  source(here::here("R", "metric_arc_threshold_error.R"))
  
  c(mean_error = metric_mean_error(n, n_noisy),
    mean_abs_error = metric_mean_abs_error(n, n_noisy),
    mean_pct_error = metric_mean_pct_error(n, n_noisy, drop_zeros),
    means_abs_pct_error = metric_mean_abs_pct_error(n, n_noisy, drop_zeros),
    median_error = metric_median_error(n, n_noisy),
    median_abs_error = metric_median_abs_error(n, n_noisy),
    median_pct_error = metric_median_pct_error(n, n_noisy, drop_zeros),
    median_abs_pct_error = metric_median_abs_pct_error(n, n_noisy, drop_zeros),
    rmse = metric_rmse(n, n_noisy),
    cv = metric_cv(n, n_noisy),
    threshold_error = metric_threshold_error(n, n_noisy, threshold, drop_zeros),
    mean_arc_pct_error = metric_mean_arc_pct_error(n, n_noisy, drop_zeros),
    mean_abs_arc_pct_error = metric_mean_abs_arc_pct_error(n, n_noisy, drop_zeros),
    metric_median_arc_pct_error = metric_median_arc_pct_error(n, n_noisy, drop_zeros),
    metric_median_abs_arc_pct_error = metric_median_abs_arc_pct_error(n, n_noisy, drop_zeros),
    arc_threshold_error = metric_arc_threshold_error(n, n_noisy, threshold, drop_zeros))

}
