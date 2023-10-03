#' Add probability of opt in (household level) to starting data
#'
#' @param starting_data A dataframe of starting data
#' @param prob A number in \[0, 1\] for the probability of opting in
#' @param white_multiplier A multiplier for the white population's probability 
#' of opting in. All probabilities are normalized using the data so the mean is 
#' prob
#' 
#' @return A dataframe of starting data with probability column added
#'
add_prob_opt_in <- function(starting_data, prob, white_multiplier = NULL) {
  
  if (is.null(white_multiplier) | prob == 1) {
    
    data <- starting_data |>
      dplyr::mutate(prob_opt_in = prob)
    
  } else  {
    
    data <- starting_data |>
      dplyr::mutate(prob_opt_in = dplyr::if_else(race_simple == "White", white_multiplier, 1)) |>
      dplyr::mutate(prob_opt_in = prob_opt_in * (prob / mean(prob_opt_in))) |>
      dplyr::mutate(prob_opt_in = pmin(prob_opt_in, 1))

  }
  
  return(data)
  
}
