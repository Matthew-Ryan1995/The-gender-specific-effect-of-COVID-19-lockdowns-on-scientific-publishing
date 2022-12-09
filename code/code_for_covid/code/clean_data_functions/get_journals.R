get_journals <- function(pubmed_df){
  require(tidyverse, quietly = TRUE)
  
  journals <- read_delim("raw_data/scimagojr 2019.csv", delim = ";", col_types = cols())
  
  # Clean journal names for matching
  # Steps:
  ## Get journal title, h-index, and category of journal
  ## Separate out each category
  ## Remove some annoying characters from the categories
  ## Get the distinct categories for each journal
  ## Count how many times each category appears across the 30 000 ish journals
  ## Keep the most popular category for each journal - Taking this a the journal discipline
  ## Clean the journal name for matching, includes removing special characters and the word "the"
  sub_journals <- journals %>% 
    select(Title, `H index`, Categories) %>% 
    separate_rows(Categories, sep = ";") %>% 
    mutate(Categories = str_remove(Categories, "(Q1)"),
           Categories = str_extract(Categories, "[^(]+"), 
           Categories = str_remove(Categories, "[$ ]*")) %>% 
    distinct(Title, Categories, .keep_all = T) %>% 
    add_count(Categories) %>% 
    group_by(Title) %>% 
    filter(n == max(n)) %>% 
    ungroup() %>% 
    rename(journal_match = Title) %>% 
    janitor::clean_names() %>% 
    select(-n) %>% 
    mutate(journal_match = str_to_lower(journal_match)) %>% 
    mutate(journal_match = str_remove(journal_match, "the"),
           journal_match = str_remove_all(journal_match, "[^[:alnum:]]"))
  
  pubmed_df <- pubmed_df %>% 
    mutate(journal_match = str_to_lower(journal),
           journal_match = str_remove(journal_match, "[.]"),
           journal_match = str_remove(journal_match, "the"),
           journal_match = str_extract(journal_match, "[^:]*"),
           journal_match = str_extract(journal_match, "[^(]+"),
           journal_match = str_extract(journal_match, "[^=]+"),
           journal_match = str_replace(journal_match, "&amp;", "and"),
           journal_match = str_remove_all(journal_match, "[^[:alnum:]]"))
  
  pubmed_matched <- left_join(pubmed_df, sub_journals, by = "journal_match") %>% 
    select(-journal_match) %>%
    rename(journal_h_index = h_index)
  
  return(pubmed_matched)
}