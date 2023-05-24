#' Add opt in decision to starting data
#'
#' @param starting_data A dataframe of starting data
#' @param prob_opt_in Name of opt in probability column
#' @param threshold Probability threshold
#' 
#' @return A dataframe of starting data with opt in decision added
#'
add_opt_in <- function(starting_data,
                       prob_opt_in,
                       threshold) {

  new_df <- starting_data |>
    dplyr::mutate(opt_in = prob_opt_in >= threshold)
  
  return(new_df)
  
}
