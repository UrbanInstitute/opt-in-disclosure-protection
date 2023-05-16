#' Add probability of opt in (household level) to starting data
#'
#' @param starting_data A dataframe of starting data
#' 
#' @return A dataframe of starting data with probability column added
#'
add_prob_opt_in <- function(starting_data) {
  
  households <- starting_data |>
    dplyr::select(serialno) |>
    dplyr::distinct()
  
  households$prob_opt_in <- runif(nrow(households))
  
  new_df <- starting_data |>
    dplyr::full_join(households, by = "serialno")
  
  return(new_df)
  
}
  