#' Add probability of opt in (household level) to starting data
#'
#' @param starting_data A dataframe of starting data
#' @param prob The type of opt-in probability: NULL (default), "uniform"
#' 
#' @return A dataframe of starting data with probability column added
#'
add_prob_opt_in <- function(starting_data, prob = NULL) {
  
  households <- starting_data |>
    dplyr::select(serialno) |>
    dplyr::distinct()
  
  if (is.null(prob)) {
    households$prob_opt_in <- 1
  } else if (prob == "uniform") {
    households$prob_opt_in <- runif(nrow(households))
  }
  
  new_df <- starting_data |>
    dplyr::full_join(households, by = "serialno")
  
  return(new_df)
  
}
  