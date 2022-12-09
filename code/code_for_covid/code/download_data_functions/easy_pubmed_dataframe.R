easy_pubmed_dataframe <- function(pubmed_articles, cores = parallel::detectCores(),
                                  max_character = 0, article_list = NULL){
  require(easyPubMed, quietly = TRUE)
  require(parallel, quietly = TRUE)
  require(glue, quietly = TRUE)
  require(foreach, quietly = TRUE)
  if(!is.null(article_list)){
    dir_name <- glue("raw_data/{article_list}")
    if(!dir.exists(dir_name)){
      dir.create(dir_name)
    }
  }else{
    dir_name <- "raw_data"
  }
  
  file_name <- "easypubmed_df"
  
  if(file.exists(glue("{dir_name}/{file_name}"))){
    return(glue("{dir_name}/{file_name}"))
  }else{
    doParallel::registerDoParallel(cores = cores)
    pubmed_df <- foreach(x = pubmed_articles,
                         .packages = "easyPubMed",
                         .combine = rbind) %dopar% article_to_df(pubmedArticle = x,
                                                                  autofill = TRUE, 
                                                                  getKeywords = TRUE,
                                                                  max_chars = max_character,
                                                                  getAuthors = TRUE)
    
    # do.call(rbind, 
    #                    mclapply(pubmed_articles, article_to_df, 
    #                             autofill = TRUE, 
    #                             getKeywords = TRUE,
    #                             max_chars = max_character, 
    #                             getAuthors = TRUE, 
    #                             mc.cores = cores))
    # 
    
    saveRDS(pubmed_df, glue("{dir_name}/{file_name}"))
    return(glue("{dir_name}/{file_name}"))
  }
}
