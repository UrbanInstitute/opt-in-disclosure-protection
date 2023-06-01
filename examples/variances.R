
library(tidyverse)
options(scipen = 999)

variance_all <- function(epsilon, k, N){

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
    wsm = variance_wsm(epsilon, k, N))

}

epsilon = .01
k = 10
N = 100000

variance_all(epsilon, k, N)
