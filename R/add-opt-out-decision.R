#' Add opt out decision to starting data
#'
#' @param starting_data A dataframe of starting data
#' @param opt_out_prob Name of opt out probability column
#' @param threshold Probability threshold
#' 
#' @return A dataframe of starting data with opt out decision added
#'
add_opt_out_decision <- function(starting_data, 
                                 opt_out_prob,
                                 threshold) {

  new_df <- starting_data |>
    dplyr::mutate(opt_out_decision = if_else(opt_out_prob >= threshold, 1, 0))
  
  return(new_df)
  
}
