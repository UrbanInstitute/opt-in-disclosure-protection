#' Add opt in decision to starting data
#'
#' @param starting_data A dataframe of starting data
#' @param prob_opt_in Name of opt in probability column
#' @param threshold Probability threshold
#' 
#' @return A dataframe of starting data with opt in decision added
#'
add_opt_in <- function(starting_data,
                       prob_opt_in) {

  new_df <- starting_data |>
    dplyr::bind_cols(random_number = runif(n = nrow(starting_data))) %>%
    dplyr::mutate(opt_in = prob_opt_in < random_number) %>%
    dplyr::select(-random_number)
  
  return(new_df)
  
}
