
################################################################

library(tidyverse)
library(urbnthemes)

age_bucket <- c("Child", "Adult", "Senior")
sex_val <- c("Male", "Female")
race_simple <- c("White", "Black", "Hispanic", "Other")
lookup <- expand.grid(age_bucket, sex_val, race_simple)
attrbs <- c("age_bucket", "sex_val", "race_simple")
names(lookup) <- attrbs

data <- lookup %>%
  mutate(freq = sample(1000:10000, n())) %>%
  uncount(freq)

sample_other_id <- function(id, ids) {
  
  ids_disjoint <- ids[ids != id] 
  sample(ids_disjoint, size = 1)
  
}

grrout <- grr(data = data, lookup = lookup, epsilon = 3, attrbs = attrbs)

# compare
summ_fn <- function(input) {
  
  summ <- input %>%
    group_by(across(all_of(attrbs))) %>%
    summarize(pct = n() / nrow(input))    
  
  return(summ)
  
}

comp <- full_join(summ_fn(data), summ_fn(grrout), by = attrbs) %>%
  mutate(diff = pct.y - pct.x,
         diff_abs = abs(diff),
         diff_pct = (diff_abs / pct.x) * 100)

# pct difference
summary(comp$diff_pct)

comp %>%
  arrange(-diff_pct)

# inverse relationship: smaller group = larger pct difference noise
ggplot(comp, aes(x = pct.x, y = diff_pct)) +
  geom_point() +
  geom_smooth(method = lm)

