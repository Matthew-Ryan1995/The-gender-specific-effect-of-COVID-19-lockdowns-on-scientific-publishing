pacman::p_load(tidyverse)

# Load data
df <- read_csv("data/September_data.csv", col_types = cols())

df <- df %>% 
  filter(country == "australia", is.na(gender)) %>% # Look at unmatched gender in Australia
  select(firstname) %>% # Get only first name
  mutate(n = map(firstname, str_split, pattern = " "),
         firstname = map_chr(n, ~.x[[1]][1]), # Simplify to first entry
         n = map_dbl(n, ~str_count(.x[[1]][1]))) %>%  # Which ones start with initials
  filter(n > 1) %>% # Remove only initial first names
  distinct(firstname) # Find the distinct names

# total number of names to consider
total_names <- nrow(df)

# Step size, genderize blocks at 1000 a day, use 900 to be safe
block_length <- 900

# Total number of steps to take
num_steps <- ceiling(total_names/block_length)

# break into small chunks to put into genderize
for(i in 1:num_steps){
  start_point <- 1 + (i - 1) * block_length
  end_point <- i * block_length
  if(end_point > total_names){
    end_point <- total_names
  }
  sub_names <- df %>% 
    slice(start_point:end_point)
  write_csv(sub_names, glue::glue("data/genderize/unmatched_names_{start_point}-{end_point}.csv"))
}




