join_dateframes <- function(pubmed_df, ris_df, article_list = NULL){
  require(tidyverse, quietly = TRUE)
  require(glue, quietly = TRUE)
  pubmed_df <- readRDS(pubmed_df)
  ris_df <- readRDS(ris_df)
  
  joint_df <- left_join(pubmed_df, ris_df, by = "pmid")
  
  if(!is.null(article_list)){
    dir_name <- glue("raw_data/{article_list}")
    if(!dir.exists(dir_name)){
      dir.create(dir_name)
    }
  }else{
    dir_name <- "raw_data"
  }
  
  data_version <- list.files(dir_name) %>% 
    str_extract("raw_pubmed") %>% 
    na.omit() %>% 
    length()
  
  data_version <- data_version + 1
  
  readr::write_csv(joint_df, glue("{dir_name}/raw_pubmed_v{data_version}.csv"))
  return(glue("{dir_name}/raw_pubmed_v{data_version}.csv"))
}