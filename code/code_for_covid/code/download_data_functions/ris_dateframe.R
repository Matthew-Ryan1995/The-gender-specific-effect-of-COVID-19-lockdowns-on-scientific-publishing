ris_dataframe <- function(pubmed_df, article_list = NULL){
  require(RISmed, quietly = FALSE)
  require(tidyverse, quietly = TRUE)
  require(glue, quietly = TRUE)
  # source("code/RISmed_changes.R")
  
  source("code/RISmed_functions/EUtilsSubGet.R")
  source("code/RISmed_functions/ParseMedlineArticle.R")
  
  if(!is.null(article_list)){
    dir_name <- glue("raw_data/{article_list}")
    if(!dir.exists(dir_name)){
      dir.create(dir_name)
    }
  }else{
    dir_name <- "raw_data"
  }
  
  file_name <- "ris_df"
  
  # if(file.exists(glue("{dir_name}/{file_name}"))){
  #   return(glue("{dir_name}/{file_name}"))
  # }else{
  
  pubmed_df <- readRDS(pubmed_df)
  
  pmid_list <- pubmed_df %>% 
    pull(pmid) %>% 
    unique() %>% 
    str_c("[PMID]")
  
  ris_df <- map_dfr(1:round(length(pmid_list)/100), 
                    function(i){
                      if(i == round(length(pmid_list)/100)){
                        pmid_query <- str_c(pmid_list[(1 + (i-1)*100):length(pmid_list)],
                                            collapse = " OR ")
                      }else{
                        pmid_query <- str_c(pmid_list[(1 + (i-1)*100):(100 + (i-1)*100)],
                                            collapse = " OR ")
                      }
                      ris_query <- EUtilsSummary(pmid_query)
                      ris_get <-EUtilsGet(ris_query)
                      
                      # Using RISmed, get the date the article was received, and the publication status
                      tmp_df <- tibble(pmid = PMID(ris_get),
                                       year_received = YearReceived(ris_get),
                                       month_received = MonthReceived(ris_get),
                                       day_received = DayReceived(ris_get),
                                       year_pub_ris = YearPubDate(ris_get),
                                       month_pub_ris = MonthPubDate(ris_get),
                                       day_pub_ris = DayPubDate(ris_get),
                                       pub_stat = PublicationStatus(ris_get),
                                       pub_type = map_chr(PublicationType(ris_get), 
                                                          ~str_c(.x, collapse = ", "))) %>% 
                        mutate(month_pub_ris = map_dbl(month_pub_ris,
                                                       function(x){
                                                         tmp <- as.numeric(x)
                                                         if(is.na(tmp) & !is.na(x)){
                                                           return(which(str_to_lower(month.abb) == str_to_lower(x)))
                                                         }else{
                                                           return(tmp)
                                                         }
                                                       }))
                      return(tmp_df)
                    } )
  
  
  saveRDS(ris_df, glue("{dir_name}/{file_name}"))
  return(glue("{dir_name}/{file_name}"))
  # }
}
