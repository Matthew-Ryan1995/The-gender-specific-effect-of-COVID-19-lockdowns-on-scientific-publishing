basic_cleaning <- function(raw_data){
  require(tidyverse, quietly = TRUE)
  
  pubmed_df <- read_csv(raw_data, col_types = cols())
  
  pubmed_df_cleaning <- pubmed_df %>% 
    select(-email) %>% 
    group_by(pmid) %>% 
    mutate(num_authors = length(title), order = row_number()) %>% 
    filter(order == max(order) | order == min(order)) %>% 
    mutate(order = ifelse(order == 1, "First", "Last"),
           order = ifelse(num_authors == 1, "Single", order)) %>% 
    ungroup() %>%  
    rename_at(vars(year:day), ~str_c(.x, "_pub")) %>% 
    mutate(across(c(starts_with("year"), starts_with("day"), starts_with("month")), as.numeric)) %>% 
    mutate(year = case_when(!is.na(year_received) ~ year_received,
                            is.na(year_received)&(!is.na(year_pub_ris)) ~ year_pub_ris,
                            TRUE ~ year_pub),
           month = case_when(!is.na(month_received) ~ month_received,
                             is.na(month_received)&(!is.na(month_pub_ris)) ~ month_pub_ris,
                             TRUE ~ as.numeric(month_pub)),
           day = case_when(!is.na(day_received) ~ day_received,
                           is.na(day_received)&(!is.na(day_pub_ris)) ~ day_pub_ris,
                           TRUE ~ as.numeric(day_pub)))
  
  return(pubmed_df_cleaning)
}