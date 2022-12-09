pacman::p_load(tidyverse, glue)

dir_name <- "final_data"

if(!dir.exists(glue("data/{dir_name}"))){
  dir.create(glue("data/{dir_name}"))
}

# Original Batch Download
version <- "1"

data_folders <- list.dirs("data/original_download")[str_detect(list.dirs("data/original_download/"), "downloaded")]

data_csvs <- glue("{data_folders}/pubmed_cleaned_v{version}.csv")

col_specs <- "dcclddcccccccddddddccdcdddccccdcc"

final_df <- map_dfr(data_csvs, read_csv, col_types = col_specs)

final_df <- final_df %>% 
  mutate(across(c(day, month, month_pub, day_pub), as.numeric)) %>% 
  select(-abstract)

write_csv(final_df, glue("data/{dir_name}/full_data.csv"))


# New Batch Download - 21 Jan 2020

version <- "1"

new_data_folders <- list.dirs("data/new_download")[str_detect(list.dirs("data/new_download/"), "downloaded")]

new_data_csvs <- glue("{new_data_folders}/pubmed_cleaned_v{version}.csv")

col_specs <- "dcclddcccccccddddddccdcdddccccdcc"

new_df <- map_dfr(new_data_csvs, read_csv, col_types = col_specs)

new_df <- new_df %>% 
  mutate(across(c(day, month, month_pub, day_pub), as.numeric)) %>% 
  select(-abstract)

final_df_v2 <- full_join(final_df, new_df) # Join both older data and new data

# do some more gender matching, because something seems to be dodgy about the pipeline
pubmed_df <- final_df_v2 %>% 
  filter(is.na(gender)) %>% 
  mutate(splitname = str_split(firstname, boundary("word"))) %>% 
  unnest(splitname) 

gender_test <- gender::gender(pubmed_df$splitname) %>% 
  select(splitname = name, gender) %>% 
  distinct(splitname, .keep_all = TRUE)

pubmed_gender_matched <- left_join(pubmed_df, gender_test, by = "splitname") %>% 
  select(-splitname) %>%
  group_by(pmid, firstname, lastname) %>% 
  mutate(gender_value = row_number()) %>% 
  filter(gender_value <= 3) %>% 
  mutate(gender_value = str_c("gender_match_", gender_value)) %>% 
  ungroup() %>% 
  pivot_wider(values_from = gender.y, names_from = "gender_value") %>%
  mutate(gender = case_when(!is.na(gender_match_1) ~ gender_match_1,
                            is.na(gender_match_1) & !is.na(gender_match_2) ~ gender_match_2,
                            TRUE ~ gender_match_3)) %>% 
  select(-gender_match_1:-gender_match_3)

## Fix gender and year
final_df_v2_gm <- pubmed_gender_matched %>% 
  select(-gender.x) %>% 
  rename(new_gender = gender) %>% 
  right_join(final_df_v2) %>% 
  mutate(gender = ifelse(is.na(gender), new_gender, gender),
         year = pmap_dbl(list(year_pub, year_received, year_pub_ris), min, na.rm = TRUE)) %>% 
  select(-new_gender)

# Fix month
month_now <- str_extract(date(),str_c(month.abb, collapse = "|"))
month_idx <- which(month.abb == month_now)

final_df_v2_gm <- final_df_v2_gm %>% 
  mutate(month = pmap_dbl(list(year, month, month_pub, month_received, month_pub_ris),
                          function(y, m, ...){
                            if(y == 2021){
                              if(m>month_idx){
                                return(min(..., na.rm = TRUE))
                              }else{
                                return(m)
                              }
                            }else{
                              return(m)
                            }
                          }))

## Fixing date, get earliest date associated with paper
get_date <- function(y, m, ...){
  ARGS <- list(...)
  if(is.na(y)){
    return(NA)
  }
  
  if(is.na(m)){
    m <- max(..., na.rm = TRUE)
  }
  
  date <- str_c(y, m, "01", sep = "-")
  return(date)
}

final_df_v2_gm <- final_df_v2_gm %>% 
  mutate(date_received = pmap(list(year_received, month_received, month_pub, month_pub_ris),
                              get_date),
         date_pub = pmap(list(year_pub, month_pub, month_received, month_pub_ris),
                         get_date),
         date_pub_ris = pmap(list(year_pub_ris, month_pub_ris, month_received, month_pub),
                             get_date)) %>% 
  unnest(c(date_received, date_pub, date_pub_ris)) %>% 
  mutate(across(starts_with("date"), lubridate::as_date),
         date = pmap(list(date_received, date_pub, date_pub_ris), min, na.rm = TRUE)) %>% 
  unnest(date) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  select(-starts_with("date"))

final_df_v2_gm <- final_df_v2_gm %>% 
  arrange(year, month, day) %>% # Order so that oldest are at the front
  distinct(pmid, order, .keep_all = TRUE) # Remove any double ups


write_csv(final_df_v2_gm, glue("data/{dir_name}/full_data_v2.csv"))

# R wants to be dodgy with reading in the column types, this will retrieve the column types for read_csv
# map_chr(final_df_v2, class) %>% 
#   str_sub(start = 1, end = 1) %>% 
#   str_replace("n", "d") %>% 
#   str_c(collapse = "")
