#' Calculate a noisy histogram using laplace sanitizer
#'
#' @param data A data frame with microdata with attributes
#' @param epsilon A numeric value of epsilon
#' @param attribs A vector of attributes for the histogram
#'
#' @return A noisy histogram with n and n_noisy
#' 
hist_global <- function(data, epsilon, attribs) {
  
  source(here("R", "lap_mech.R"))
  
  # workaround to keep state identifier
  # this function is not generalized
  if(("state" %in% colnames(data)) & (length(unique(data$state)) == 1)) {
    state_id <- unique(data$state)
  }

  # create histograms -----------------------------------------------------
  histogram <- data %>%
    dplyr::group_by(dplyr::across(all_of(attribs))) %>%
    dplyr::count() %>%
    dplyr::ungroup()

  # apply noise
  data_synth <- histogram %>%
    dplyr::bind_cols(noise = lap_mech(n = nrow(histogram),
                                      eps = epsilon, 
                                      gs = 1)) %>%
    dplyr::mutate(n_noisy = n + noise) %>%
    dplyr::select(-noise)
  
  # temporary workaround to keep state identifier
  if(("state" %in% colnames(data)) & (length(unique(data$state)) == 1)) {
    data_synth$state <- state_id
  }
  
  return(data_synth)
  
}
