fetch_data <- function(query, article_list = NULL,
                       BATCH = FALSE, total_retrieve = 1000){
  require(easyPubMed, quietly = TRUE)
  require(glue, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  
  articles_to_list <- function (pubmed_data, encoding = "UTF8", simplify = TRUE){
    easyPM_exec_art_to_lst <- function(pm_dataa, simply = TRUE) {
      pm_datab <- str_split(pm_dataa, "<PubmedArticle(>|[[:space:]]+?.*>)")[[1]][-1]
      pm_datab <- sapply(pm_datab, function(x) {
        if (!str_detect(x, "</PubmedArticle>$")){
          x <- str_replace(x, "(^.*</PubmedArticle>).*$", "\\1")
        }
        x <- str_c("<PubmedArticle>", x)
        return(str_replace_all(x, "[[:space:]]{2,}", " "))
      }, USE.NAMES = FALSE, simplify = simply)
      return(pm_datab)
    }
    TMP <- str_sub(pubmed_data[1], 1, 1000)
    if (str_detect(TMP, "<PubmedArticle")) {
      out <- easyPM_exec_art_to_lst(pubmed_data[1], simply = simplify)
    }
    else if (file.exists(pubmed_data[1])) {
      con1 <- file(pubmed_data[1], encoding = "UTF8")
      on.exit(close(con1))
      myXML <- readLines(con = con1, n = -1, ok = TRUE, encoding = "UTF8")
      if (encoding != "UTF8") 
        myXML <- base::iconv(myXML, from = "UTF8", to = encoding, 
                             sub = ".")
      myXML <- str_c(myXML, collapse = "")
      out <- easyPM_exec_art_to_lst(myXML, simply = simplify)
    }
    else {
      message("An error occurred")
      return(NULL)
    }
    return(out)
  }
  
  if(!is.null(article_list)){
    pubmed_articles <- map(article_list, ~articles_to_list(glue("raw_data/batch_download/{.x}"))) %>% 
      unlist()
    return(pubmed_articles)
  }else{
    if(BATCH){ # Do a batch download.  Should get an API key to speed up
      pubmed_fetch <- batch_pubmed_download(pubmed_query_string = query, 
                                            dest_dir = "raw_data/batch_download", 
                                            dest_file_prefix = glue::glue('data_downloaded_{stringr::str_replace_all(date(), " ", "_")}'),
                                            batch_size = 5000)
      pubmed_articles <- map(pubmed_fetch, articles_to_list) %>% unlist()
    }else{ # Do it manually
      pubmed_query <- get_pubmed_ids(query)
      pubmed_fetch <- fetch_pubmed_data(pubmed_query, retmax = total_retrieve)
      pubmed_articles <- articles_to_list(pubmed_fetch)
    }
    return(pubmed_articles)
  }
}