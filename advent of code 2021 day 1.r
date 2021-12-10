library(tidyverse)
library(janitor)

df <- read.delim("input 01-21.txt", header = F)

# Part I ------------------------------------------------------------------
janitor::tabyl(
df %>%
  mutate(dir = ifelse(lag(V1)>V1, "decrease", "increase")), dir
)


# Part II -----------------------------------------------------------------
janitor::tabyl(
df %>%
  mutate(depth = V1 + lag(V1) + lag(V1, n = 2L),
         dir = case_when(lag(depth) > depth ~ "decrease",
                         lag(depth) < depth ~ "increase",
                         lag(depth) == depth ~ NA_character_)), dir
)
