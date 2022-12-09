extra_gender_matching <- function(df){
  
  df_gender_match <- df %>% 
    filter(!is.na(gender))
  
  df_unmatched <- df %>% 
    filter(is.na(gender))
  
  genderize_data_files <- list.files("data/genderize", full.names = T)
  genderize_data_files <- genderize_data_files[str_detect(genderize_data_files, "genderize_names")]
  genderize_data <- map_dfr(genderize_data_files, read_csv, col_types = cols()) %>% 
    select(match_name = firstname, gender)
  
  df_unmatched <- df_unmatched %>% 
    select(-gender) %>% 
    mutate(match_name = map(firstname, str_split, pattern = " "),
           match_name = map_chr(match_name, ~.x[[1]][1]))
  
  df_unmatched <- left_join(df_unmatched, genderize_data, by = "match_name")
  
  df_unmatched <- df_unmatched %>% 
    mutate(gender = ifelse(gender == "unknown", NA, gender))
  
  return_df <- bind_rows(df_gender_match, df_unmatched)
  

  
  return(return_df)
}