#' Calculate variance for all methods
#'
#' @param epsilon A numeric value of epsilon
#' @param k Cardinality of total values (number of attribute combinations)
#' @param N Number of observations/respondents
#'
#' @return Variance value
#' 
variance_all <- function(epsilon, k, N){
  
  source(here::here("R", "variance_brr.R"))
  source(here::here("R", "variance_grr.R"))
  source(here::here("R", "variance_sue.R"))
  source(here::here("R", "variance_oue.R"))
  source(here::here("R", "variance_rappor.R"))
  source(here::here("R", "variance_orappor.R"))
  source(here::here("R", "variance_orr.R"))
  source(here::here("R", "variance_blh.R"))
  source(here::here("R", "variance_olh.R"))
  source(here::here("R", "variance_shist.R"))
  source(here::here("R", "variance_hrr.R"))
  source(here::here("R", "variance_omega_sm.R"))
  
  c(brr = variance_brr(epsilon, k, N),
    grr = variance_grr(epsilon, k, N),
    sue = variance_sue(epsilon, k, N),
    oue = variance_oue(epsilon, k, N),
    rappor = variance_rappor(epsilon, k, N),
    orappor = variance_orappor(epsilon, k, N),
    orr = variance_orr(epsilon, k, N),
    blh = variance_blh(epsilon, k, N),
    olh = variance_olh(epsilon, k, N),
    shist = variance_shist(epsilon, k, N),
    hrr = variance_hrr(epsilon, k, N),
    omega_sm = variance_omega_sm(epsilon, k, N))

}
