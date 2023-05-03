
library(tidyverse)

# UNARY ENCODING
# https://programming-dp.com/ch13.html#unary-encoding
# follows along the python code steps

# set p, q (and epsilon)
p = .75
q = 1 - p

unary_epsilon <- function(p, q){
  return(log((p*(1-q)) / ((1-p)*q)))
}

unary_epsilon(p, q)

# load test dataset from Programming Differential Privacy
df_read <- read.csv("https://raw.githubusercontent.com/uvm-plaid/programming-dp/master/notebooks/adult_with_pii.csv")

# prep input df
opt_prob <- 0.3

df <- df_read %>%
  rename_with(tolower, everything()) %>%
  mutate(occupation = ifelse(occupation == "", "Missing", occupation)) %>%
  select(name, occupation)

df$optout <- rbinom(n = nrow(df_read), 
                    size = 1, 
                    prob = opt_prob)

# create domain
domain <- unique(df$occupation)
domain

# function for encoding
encode <- function(response, domain){
  return(ifelse(response == domain, 1, 0))
}

encode('Sales', domain)

# function for perturbing
perturb <- function(bit){
  
  sample <- runif(1)
  
  if (bit == 1){
    if (sample <= p){
      return(1)
    }
    else{
      return(0)
    }
  }
  
  else if (bit == 0){
    if (sample <= q){
      return(1)
    }
    else{
      return(0)
    }
  }
}
perturb_vec <- Vectorize(perturb)

perturb_vec(encode('Sales', domain))

# helper combination function
encode_perturb <- function(response, domain){
  return(perturb_vec(encode(response, domain)))
}

# split df
df_privacy <- df[df$optout == 0,]
df_optout <- df[df$optout == 1,]

# true counts
count_true <- tibble(occupation = domain, 
                     count_true_optout = Reduce("+", lapply(df_optout$occupation, encode, domain)),
                     count_true_privacy = Reduce("+", lapply(df_privacy$occupation, encode, domain)),
                     count_true = count_true_privacy + count_true_optout)

# aggregate and adjust
responses_optout <- lapply(df_privacy$occupation, encode_perturb, domain)

agg_adjust <- function(responses, p, q){
  
  sums <- Reduce("+", responses)
  n = length(responses)
  
  vec <- c()
  for (sum in sums){
    vec <- append(vec, (sum - n*q) / (p-q) )
  }
  
  return(vec)
}

count_perturbed <- tibble(occupation = domain, 
                          count_true_optout = Reduce("+", lapply(df_optout$occupation, encode, domain)),
                          count_perturbed_privacy = agg_adjust(responses_optout, p, q),
                          count_privacy = count_true_optout + count_perturbed_privacy)

compare <- tibble(occupation = domain,
                  count_true = Reduce("+", lapply(df$occupation, encode, domain)),
                  count_true_optout = Reduce("+", lapply(df_optout$occupation, encode, domain)),
                  count_perturbed_privacy = agg_adjust(responses_optout, p, q),
                  count_privacy = count_true_optout + count_perturbed_privacy,
                  pct_diff = (abs(count_privacy - count_true) / count_true) * 100)

compare
summary(compare$pct_diff)

# negative counts -> post processing?
