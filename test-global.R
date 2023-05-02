
library(tidyverse)
library(ExtDist)

# global approach
# https://programming-dp.com/ch4.html?highlight=histogram#histograms

# load and prep test dataset from Programming Differential Privacy
df_read <- read.csv("https://raw.githubusercontent.com/uvm-plaid/programming-dp/master/notebooks/adult_with_pii.csv")

df <- df_read %>%
  rename_with(tolower, everything()) %>%
  select(name, education)

# prep counts by education
counts <- df %>%
  group_by(education) %>%
  summarize(true_counts = n()) %>%
  arrange(-true_counts)

# note: histograms automatically satisfy parallel composition

# set epsilon, create noise function
epsilon = .5

add_noise <- function(x){
  return(x + rLaplace(1, mu = 0, b = 1/epsilon))
}
add_noise_vec <- Vectorize(add_noise)

# generate noisy counts
counts$noisy_counts <- sapply(counts$true_counts, add_noise_vec)

# compare
counts$pct_diff <- (abs(counts$noisy_counts - counts$true_counts) / counts$true_counts) * 100

counts
summary(counts$pct_diff)
