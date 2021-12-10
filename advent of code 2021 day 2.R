library(tidyverse)

df <- read_delim("input 02-21.txt", col_names = c("dir", "value"))


# Part I ------------------------------------------------------------------
df %>%
  split(.$dir) %>%
  map_df(., ~sum(.$value)) %>%
  summarize(ans = (down-up) * forward)


# Part II -----------------------------------------------------------------
horizontal = 0
aim = 0
depth = 0

for (i in 1:nrow(df)) {

  dir <- df[i,1]
  value <- df[i,2]

  if (dir == "forward") {
    horizontal <- horizontal + value
    depth <- depth + (aim*value)
  }

  if (dir == "up") {
    aim <- aim - value
  }

  if (dir == "down") {
    aim <- aim + value
  }

}

depth*horizontal


