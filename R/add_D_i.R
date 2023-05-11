#' Add cell IDs to data frame
#'
#' @param data An input data frame
#'
#' @return The input data frame with column, D_i 
#'
add_D_i <- function(data) {
  
  histogram <- data %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(), as.factor)) %>%
    dplyr::count(dplyr::across(dplyr::everything()), .drop = FALSE)
    
  d <- nrow(histogram)
  
  ids <- bind_cols(
    histogram,
    D_i = stringr::str_pad(1:d, width = 3, pad = "0", side = "left")
  ) %>%
    dplyr::select(-n)
  
  data %>%
    dplyr::left_join(ids)
  
}
