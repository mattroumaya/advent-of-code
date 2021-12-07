library(tidyverse)

# Part I ------------------------------------------------------------------

positions <- as.numeric(read.delim("C:/input.txt", header = F, sep = ','))         

distance_sum <- function(x) {
  
positions %>% 
  as_tibble() %>% 
  mutate(total = abs(value - x)) %>% 
  summarize(sum(total))
  
}

min(map_df(min(positions):max(positions), ~distance_sum(.)))



# Part II -----------------------------------------------------------------
diff <- function(x) {
  positions %>% 
  as_tibble() %>% 
  mutate(to = x,
         diff = abs(value - to)) %>% 
    summarize(total = sum(map_dbl(diff, ~sum(seq(1:.)))))
}

map_df(min(positions):max(positions), ~diff(.)) %>% 
  filter(total == min(total))

