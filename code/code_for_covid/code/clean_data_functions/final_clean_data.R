final_clean_data <- function(pubmed_df, institute_match, journal_match,
                             gender_match, race_match = NULL, article_list = NULL){
  require(tidyverse, quietly = TRUE)
  require(glue, quietly = TRUE)
  
  pubmed_final <- left_join(select(pubmed_df, -country), institute_match)
  pubmed_final <- left_join(pubmed_final, journal_match)
  pubmed_final <- left_join(pubmed_final, gender_match)
  
  if(!is.null(race_match)){
    pubmed_final <- left_join(pubmed_final, race_match)
  }
  
  if(!is.null(article_list)){
    dir_name <- glue("data/{article_list}")
    if(!dir.exists(dir_name)){
      dir.create(dir_name)
    }
  }else{
    dir_name <- "data"
  }
  
  data_version <- list.files(dir_name) %>% 
    str_extract("pubmed_cleaned") %>% 
    na.omit() %>% 
    length()
  
  data_version <- data_version + 1
  
  readr::write_csv(pubmed_final, glue("{dir_name}/pubmed_cleaned_v{data_version}.csv"))
  return(glue("{dir_name}/pubmed_cleaned_v{data_version}.csv"))
}
