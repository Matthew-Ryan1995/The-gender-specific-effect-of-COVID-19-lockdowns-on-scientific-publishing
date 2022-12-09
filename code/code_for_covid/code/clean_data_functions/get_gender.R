get_gender <- function(pubmed_df){
  require(tidyverse, quietly = TRUE)
  require(gender, quietly = TRUE)
  
  pubmed_df <- pubmed_df %>% 
    mutate(splitname = str_split(firstname, boundary("word"))) %>% 
    unnest(splitname) 
  
  gender_test <- gender(pubmed_df$splitname) %>% 
    select(splitname = name, gender) %>% 
    distinct(splitname, .keep_all = TRUE)
  
  pubmed_gender_matched <- left_join(pubmed_df, gender_test, by = "splitname") %>% 
    select(-splitname) %>%
    group_by(pmid, firstname, lastname) %>% 
    mutate(gender_value = row_number()) %>% 
    filter(gender_value <= 3) %>% 
    mutate(gender_value = str_c("gender_match_", gender_value)) %>% 
    ungroup() %>% 
    pivot_wider(values_from = gender, names_from = "gender_value") %>%
    mutate(gender = case_when(!is.na(gender_match_1) ~ gender_match_1,
                              is.na(gender_match_1) & !is.na(gender_match_2) ~ gender_match_2,
                              TRUE ~ gender_match_3),
           gender = as.factor(gender)) %>% 
    select(-gender_match_1:-gender_match_3)
  
  return(pubmed_gender_matched)
}