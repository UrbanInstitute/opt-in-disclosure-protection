#' Add probability of opt out to starting data
#'
#' @param starting_data A dataframe of starting data
#' 
#' @return A dataframe of starting data with probability column added
#'
add_opt_out_prob <- function(starting_data) {
  
  households <- starting_data |>
    dplyr::select(serialno) |>
    dplyr::distinct()
  
  households$opt_out_prob <- runif(nrow(households))
  
  new_df <- starting_data |>
    dplyr::full_join(households, by = "serialno")
  
  return(new_df)
  
}
  