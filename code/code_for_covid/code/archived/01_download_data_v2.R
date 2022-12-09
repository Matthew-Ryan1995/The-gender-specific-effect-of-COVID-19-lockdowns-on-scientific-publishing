## Author: Matt Ryan
## Date: 21/11/2020
## Update: 26/11/2020

## This script will download data from PubMed based on a search query
## This assumes there is a folder called raw_data in your working directory
## You need pacman and readr installed for this script to run 

# Data downloaded on 21/11/2020

# Packages
pacman::p_load(easyPubMed, RISmed, parallel, tictoc, glue, tidyverse)

tic()
# Set some parameters
BATCH <- FALSE # If TRUE, will download all articles
total_retrieve <- 1000  # How many articles do you want? Only used if BATCH = FALSE
max_character <- 0 # How many characters do you want in the abstract
cores <- 3 # How many cores to you want to use to construct the 

# What query to search?
# Current: Published on pubmed between 2019 and 2020, keyword Australia
query <- '(Australia[Affiliation]) AND (("2019"[Date - Publication] : "3000"[Date - Publication]))' 

if(BATCH){ # Do a batch download.  Should get an API key to speed up
  pubmed_fetch <- batch_pubmed_download(pubmed_query_string = query, 
                                        dest_dir = "raw_data/", 
                                        dest_file_prefix = "data_downloaded",
                                        batch_size = 5000)
}else{ # Do it manually
  pubmed_query <- get_pubmed_ids(query)
  pubmed_fetch <- fetch_pubmed_data(pubmed_query, retmax = total_retrieve)
}

# Convert the fetched data to article lists
pubmed_articles <- articles_to_list(pubmed_fetch)

# Convert the articles to a dataframe
pubmed_df <- do.call(rbind, 
                     mclapply(pubmed_articles, article_to_df, 
                              autofill = TRUE, 
                              getKeywords = TRUE,
                              max_chars = max_character, 
                              getAuthors = TRUE, 
                              mc.cores = cores) )

# Pull PMID to search using RISmed
pmid_list <- pubmed_df %>% 
  pull(pmid) %>% 
  unique() %>% 
  str_c("[PMID]") #, collapse = " OR ")

# Query RISmed to find the PMID
ris_df <- map_dfr(1:10, 
                  function(i){
                    pmid_query <- str_c(pmid_list[(1 + (i-1)*100):(100 + (i-1)*100)],
                                        collapse = " OR ")
                    ris_query <- EUtilsSummary(pmid_query)
                    ris_get <-EUtilsGet(ris_query)
                    
                    # Using RISmed, get the date the article was received, and the publication status
                    tmp_df <- tibble(pmid = PMID(ris_get),
                                     year_received = YearReceived(ris_get),
                                     month_received = MonthReceived(ris_get),
                                     day_received = DayReceived(ris_get),
                                     pub_stat = PublicationStatus(ris_get),
                                     pub_type = map_chr(PublicationType(ris_get), ~str_c(.x, collapse = ", ")))
                    return(tmp_df)
                  } )

# Join the data from both search engines

joint_df <- left_join(pubmed_df, ris_df, by = "pmid")

readr::write_csv(joint_df, "raw_data/raw_pubmed_v2.csv")

t <- toc(quiet = T)
print(glue("That took {round(t$toc - t$tic, 3)} seconds."))
