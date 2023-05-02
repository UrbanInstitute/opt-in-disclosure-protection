
library(tidyverse)
library(ggplot2)

# UNARY ENCODING
# https://programming-dp.com/ch13.html#unary-encoding

# epsilon function
unary_epsilon <- function(p){
  return(log((p*(1-(1-p))) / ((1-p)*(1-p))))
}

# function for encoding
encode <- function(response, domain){
  return(ifelse(response == domain, 1, 0))
}

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
    if (sample <= (1-p)){
      return(1)
    }
    else{
      return(0)
    }
  }
}
perturb_vec <- Vectorize(perturb)

# helper combination function
encode_perturb <- function(response, domain){
  return(perturb_vec(encode(response, domain)))
}

# aggregate and adjust
agg_adjust <- function(responses, p){
  
  sums <- Reduce("+", responses)
  n = length(responses)
  
  vec <- c()
  for (sum in sums){
    vec <- append(vec, (sum - n*(1-p)) / (p-(1-p)) )
  }
  
  return(vec)
}

# overall unary function
unary <- function(opt_prob, p, df){
  
  # note: right now the function has df$agg_group hard coded as the grouping variable
  
  # printing epsilon
  print(unary_epsilon(.75))
  
  # create opt out rate probability
  df$optout <- rbinom(n = nrow(df), size = 1, prob = opt_prob)
  
  # domain of possible group values
  domain <- unique(df$agg_group)
  
  # split df for opt outs vs. full privacy
  df_privacy <- df[df$optout == 0,]
  df_optout <- df[df$optout == 1,]
  
  # create vector of perturbed encoded responses
  responses_optout <- lapply(df_privacy$agg_group, encode_perturb, domain)
  
  # true counts by group
  count_true = Reduce("+", lapply(df$agg_group, encode, domain))
  
  # true counts for those who opt out
  if(nrow(df_optout) > 0){
    count_true_optout = Reduce("+", lapply(df_optout$agg_group, encode, domain))
  }
  else{
    count_true_optout = rep(0, length(domain))
  }
  
  # perturbed counts for those who do not opt out
  if(nrow(df_privacy) > 0){  
    count_perturbed_privacy = agg_adjust(responses_optout, p)
  }
  else{
    count_perturbed_privacy = rep(0, length(domain))
  }

  # storing everything in a tibble for comparisons
  compare <- tibble(occupation = domain,
                    count_true = count_true,
                    count_true_optout = count_true_optout,
                    count_perturbed_privacy = count_perturbed_privacy,
                    count_privacy = count_true_optout + count_perturbed_privacy,
                    pct_diff = (abs(count_privacy - count_true) / count_true) * 100)
  
  return(compare)
  
}

##########

# set p
p <- 0.75

# prep dataset from Programming Differential Privacy
df_read <- read.csv("https://raw.githubusercontent.com/uvm-plaid/programming-dp/master/notebooks/adult_with_pii.csv")

# right now the function has df$agg_group hard coded, so need to rename here
df <- df_read %>%
  rename_with(tolower, everything()) %>%
  mutate(occupation = ifelse(occupation == "", "Miss", occupation)) %>%
  select(name, occupation) %>%
  rename(agg_group = occupation)

# iterate over opt out rates
opt_out_rates <- seq(0, 1, by = 0.05)
results <- lapply(opt_out_rates, unary, .75, df)
med_pct_diff <- sapply(results, function(x) {median(x$pct_diff)})

ggplot() + 
  geom_point(aes(x = opt_out_rates, y = med_pct_diff))
