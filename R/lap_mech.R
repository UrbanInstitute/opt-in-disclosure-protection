#' Laplace mechanism
#'
#' @param n The number of draws
#' @param epsilon epsilon, privacy loss (num)
#' @param gs L1 global sensitivity of the specific query (num)
#'
#' @return A vector of draws from the lap. dist. that satisfies approx-DP (num)
#' 
lap_mech <- function(n, epsilon, gs) {
  
  # Calculating the scale
  scale <- gs / epsilon
  
  r <- stats::runif(n)
  
  x <- ifelse(
    test = r > 0.5,
    yes = 0 - sign(r - 0.5) * scale * log(2 * (1 - r)),
    no = 0 - sign(r - 0.5) * scale * log(2 * r)
  )
  
  return(x)
  
}
