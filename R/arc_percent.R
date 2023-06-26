#' Calculate a vector of arc percentage changes
#'
#' @param x A numeric vector
#' @param y A numeric vector
#'
#' @return Vector of numeric arc percentage changes
#' 
arc_percent <- function(x, y) {
  
  arc_percent <- dplyr::if_else(
    x == y,
    0,
    (x - y) / (0.5 * (x + y))
  )
  
  return(arc_percent)
  
}
