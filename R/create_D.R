#' Create a vector of IDs with each element corresponding to a cell in the 
#' histogram
#' 
#' This includes all possible cases
#'
#' @param data A data frame
#'
#' @return A vector of IDs
#'
create_D <- function(data) {
  
  histogram <- data %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(), as.factor)) %>%
    dplyr::count(dplyr::across(dplyr::everything()), .drop = FALSE)
  
  d <- nrow(histogram)
  
  D <- stringr::str_pad(1:d, width = 3, pad = "0", side = "left")
  
  return(D)
  
}
