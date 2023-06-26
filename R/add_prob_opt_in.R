#' Add probability of opt in (household level) to starting data
#'
#' @param starting_data A dataframe of starting data
#' @param prob A number in \[0, 1\] for the probability of opting in
#' @param white_multiplier A multiplier for the white probability of opting in
#' All probabilities are normalized using the data so the mean is prob
#' 
#' @return A dataframe of starting data with probability column added
#'
add_prob_opt_in <- function(starting_data, prob, white_multiplier = NULL) {
  
  if (is.null(white_multiplier)) {
    
    data <- starting_data |>
      dplyr::mutate(prob_opt_in = prob)
    
  } else  {
    
    data <- starting_data |>
      dplyr::mutate(prob_opt_in = white_multiplier * (1 + (race_simple == "White"))) |>
      dplyr::mutate(prob_opt_in = prob_opt_in * (prob / mean(prob_opt_in)))

  }
  
  return(data)
  
}
  