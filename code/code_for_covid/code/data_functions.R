## Author: Matt Ryan
## Date: 21/11/2020
## Update: 26/11/2020

## This script will download data from PubMed based on a search query
## This assumes there is a folder called raw_data in your working directory
## You need pacman and readr installed for this script to run 

# Data downloaded on 21/11/2020

# Packages
download_data <- function(query, BATCH = FALSE, total_retrieve = 1000,
                          max_character = 0, cores= 3){
  pacman::p_load(easyPubMed, RISmed, parallel, glue)
  # Set some parameters
  # BATCH <- FALSE # If TRUE, will download all articles
  # total_retrieve <- 1000  # How many articles do you want? Only used if BATCH = FALSE
  # max_character <- 0 # How many characters do you want in the abstract
  # cores <- 3 # How many cores to you want to use to construct the 
  
  # What query to search?
  # Current: Published on pubmed between 2019 and 2020, keyword Australia
  #query <- '(Australia[Affiliation]) AND (("2019"[Date - Publication] : "3000"[Date - Publication]))' 
  
  if(BATCH){ # Do a batch download.  Should get an API key to speed up
    pubmed_fetch <- batch_pubmed_download(pubmed_query_string = query, 
                                          dest_dir = "raw_data/", 
                                          dest_file_prefix = "data_downloaded",
                                          batch_size = 5000)
    pubmed_articles <- map(pubmed_fetch, articles_to_list) %>% unlist()
  }else{ # Do it manually
    pubmed_query <- get_pubmed_ids(query)
    pubmed_fetch <- fetch_pubmed_data(pubmed_query, retmax = total_retrieve)
    pubmed_articles <- articles_to_list(pubmed_fetch)
  }
  
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
    str_c("[PMID]")
  
  # Query RISmed to find the PMID
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
                                       pub_stat = PublicationStatus(ris_get),
                                       pub_type = map_chr(PublicationType(ris_get), ~str_c(.x, collapse = ", ")))
                      return(tmp_df)
                    } )
  
  # Join the data from both search engines
  
  joint_df <- left_join(pubmed_df, ris_df, by = "pmid")
  
  data_version <- list.files("raw_data/") %>% 
    str_extract("raw_pubmed") %>% 
    na.omit() %>% 
    length()
  
  data_version <- data_version + 1
  
  readr::write_csv(joint_df, glue("raw_data/raw_pubmed_v{data_version}.csv"))
  return(glue("raw_data/raw_pubmed_v{data_version}.csv"))
}

clean_data <- function(raw_data){
  pacman::p_load(tidyverse, easyPubMed, gender, predictrace, glue)
  
  ## Read in and ready data
  pubmed_df <- read_csv(raw_data) # This is the data generated by "01_download_data"
  
  # Load in a list of 142 countries from the gapminder package
  # Manually add in some missing countries/abbreviations
  countries <- unique(gapminder::gapminder$country) %>% 
    as.character()%>% 
    str_extract("[^,]*")
  countries[143] <- "USA"
  countries[144] <- "UK"
  countries[145] <- "Estonia"
  countries[146] <- "Qatar"
  countries[146] <- "Fiji"
  countries[147] <- "Brasil"
  
  # Convert to a logical string statement
  countries <- str_to_lower(str_c(countries, collapse = "|") )
  
  # Read in a list educational institutes in Australia courtesy of
  # https://data.gov.au/data/dataset/tqnr/resource/07370e3f-780b-4a70-8c87-b6796d5ab237
  institute_names <- read_csv("raw_data/providers_19112020a.csv", col_types = cols()) %>% 
    select(ProviderName, Category)
  
  # Clean institute names
  institute_names_list <- institute_names %>% 
    filter(Category == "Australian University") %>% # Filter to get universities only
    mutate(ProviderName = str_remove(ProviderName, "Pty"),
           ProviderName = str_remove(ProviderName, "Ltd"), # Remove some annoying characters
           ProviderName = str_remove(ProviderName, "The "),
           ProviderName = str_extract(ProviderName, "[^(]+"),
           ProviderName = str_remove(ProviderName, "Limited"),
           ProviderName = str_remove(ProviderName, "[.]")) %>% 
    mutate(ProviderName = case_when(
      ProviderName == "Torrens University Australia " ~"Torrens University",
      ProviderName == "University of Notre Dame Australia" ~ "University of Notre Dame",
      ProviderName == "Federation University Australia " ~ "Federation University",
      TRUE ~ ProviderName)) %>% 
    pull(ProviderName)
  # Add Other names
  institute_names_list[41] <- "UniSA"
  institute_names_list[42] <- "UNSW"
  institute_names_list[43] <- "RMIT"
  institute_names_list[44] <- "Sunshine Coast University"
  institute_names_list[45] <- "Gold Coast University"
  institute_names_list[46] <- "CSIRO"
  
  institute_list <- tibble(name = institute_names_list,
                           match_name = institute_names_list %>% 
                             str_to_lower() %>% 
                             str_remove_all(str_c(PubMed_stopwords, collapse = "|")) %>% 
                             str_remove_all("[^[:alnum:]]"))
  
  # Convert to a logical string statement
  # institute_names_list <- str_c(str_to_lower(institute_names_list), collapse = "|")
  institute_names_list <- institute_list$match_name %>%
    str_c(collapse = "|")
  
  # Read in a list of journals obtained from https://www.scimagojr.com/journalrank.php
  journals <- read_delim("raw_data/scimagojr 2019.csv", delim = ";", col_types = cols())
  
  # Clean journal names for matching
  # Steps:
  ## Get journal title, h-index, and category of journal
  ## Seperate out each category
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
  
  # Clean city names for matching country based on city name
  # source : https://simplemaps.com/data/au-cities
  aus_all <- read_csv("raw_data/au.csv", col_types = cols())
  
  # Remove cities found to manually cause problems
  aus_all <- aus_all %>% 
    filter(!(city %in% c("York", "Sheffield", "Warwick", "Weston",
                         "Monash", "Griffith", "Stirling", "Ingham",
                         "Alexandra", "Richmond", "Churchill", "Wellington")))
  
  # Create search data for each city and state
  aus_cities <- aus_all$city
  aus_states_abbrv <- c("SA", "Vic", "VIC", "NSW", "ACT", "WA", "TAS", "QLD", "NT")
  aus_state <- c("South Australia", "Victoria", "Victoria", "New South Wales", 
                 "Australian Capital Territory", "Western Australia", "Tasmania",
                 "Queensland", "Northern Territory")
  
  aus_states <- tibble(state = aus_state,
                       abbr = aus_states_abbrv)
  
  # Change to logical string searches
  aus_cities <- str_c((aus_cities), collapse = "|")
  aus_states_abbrv <- str_c((aus_states_abbrv), collapse = "|")
  aus_state <- str_c((aus_state), collapse = "|")
  
  
  ## Perform the first step of cleaning
  # Drop the email field
  # Add the order of authors and the total number of authors on a paper
  # Select only first and last authors
  # Convert numeric ordering to First author, Last author, or Single author
  # Extract all the names making up firstname i.e. initials etc.
  # Extract country from address, THIS IS NOT PERFECT
  # Extract institute from address, THIS IS NOT PERFECT
  # Rename institute to something readable
  # Merge dates: If date received is available, use that, otherwise use data published
  # Extract cities and states, identify country based on city, collapse states
  
  pubmed_df_cleaning <- pubmed_df %>% 
    select(-email) %>% 
    group_by(pmid) %>% 
    mutate(num_authors = length(title), order = row_number()) %>% 
    filter(order == max(order) | order == min(order)) %>% 
    mutate(order = ifelse(order == 1, "First", "Last"),
           order = ifelse(num_authors == 1, "Single", order)) %>% 
    ungroup() %>% 
    mutate(splitname = str_split(firstname, boundary("word")),
           country = str_extract(str_to_lower(address), 
                                 countries),
           institute = address %>% 
             str_to_lower() %>% 
             str_remove_all(str_c(PubMed_stopwords, collapse = "|")) %>% 
             str_remove_all("[^[:alnum:]]") %>% 
             str_extract(institute_names_list),
           institute = map_chr(institute, function(str){
             if(str %in%institute_list$match_name){
               return(institute_list$name[map_lgl(institute_list$match_name,
                                                  function(name){
                                                    return(str==name)
                                                  })])
             }else{
               return(NA)
             }
           }),
           institute = case_when(institute == "RMIT" ~ "Royal Melbourne Institute of Technology",
                                 institute == "UniSA" ~ "University of South Australia",
                                 institute == "Sunshine Coast University" ~ "University of the Sunshine Coast",
                                 institute == "UNSW" ~ "University of New South Wales",
                                 TRUE ~ institute)) %>% 
    unnest(splitname) %>% 
    rename_at(vars(year:day), ~str_c(.x, "_pub")) %>% 
    mutate(year = ifelse(!is.na(year_received), year_received, year_pub),
           month = ifelse(!is.na(month_received), month_received, month_pub),
           day = ifelse(!is.na(day_received), day_received, day_pub)) %>% 
    mutate(
      city = str_extract(address, aus_cities),
      state_abr = str_extract((address), aus_states_abbrv),
      state = str_extract((address), aus_state),
      state = case_when(is.na(state) ~ map_chr(state_abr,
                                               function(str){
                                                 if(str %in% aus_states$abbr){
                                                   return(aus_states$state[
                                                     map_lgl(aus_states$abbr,
                                                             function(name){
                                                               return(str==name)
                                                             })
                                                   ])
                                                 }else{
                                                   return(NA)
                                                 }
                                               }),
                        TRUE ~ state),
      city_state = map_chr(city,
                           function(city_name){
                             if(!is.na(city_name)){
                               tmp <- aus_all %>% 
                                 distinct(city, .keep_all = T) %>% 
                                 filter(city == city_name) %>% 
                                 pull(admin_name)
                               return(tmp)
                             }else{
                               return(NA)
                             }
                           }),
      state = case_when(is.na(state) ~ city_state,
                        state != city_state ~ city_state,
                        TRUE ~ state),
      one_identified = !is.na(state_abr) | !is.na(state) | !is.na(institute),
      country = case_when((is.na(country) & one_identified) ~ "australia",
                          TRUE ~ country)
    ) %>% 
    select(-state_abr, -one_identified, -city_state) 
  
  # Use the gender package to gender match based on any of the first names, remove any repeats
  gender_test <- gender(pubmed_df_cleaning$splitname) %>% 
    select(splitname = name, gender) %>% 
    distinct(splitname, .keep_all = TRUE)
  
  # Join the gender matched names to the cleaned data
  # Find the gender matching based on the first 3 choices of first name
  # assign gender on the first appearing gender
  pubmed_gender_matched <- left_join(pubmed_df_cleaning, gender_test, by = "splitname") %>% 
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
  
  # Use the predictrace package to predict race based on last name
  race_test <- predict_race(pubmed_gender_matched$lastname, probability = FALSE) %>% 
    select(lastname = name, race = likely_race) %>% 
    distinct(lastname, .keep_all = TRUE)
  
  # Join the race matched data to the gender matched data
  pubmed_race_matched <- left_join(pubmed_gender_matched, race_test, by = "lastname")
  
  # Create a variable to match journals on
  # Clean the variable to make for more successful matching
  # Steps:
  ## Convert to lower case
  ## Remove full stops
  ## Remove "the" from the beginning of journals
  ## Remove anything after a colon
  ## Remove anything in brackets
  ## Remove anything after an equals sign
  ## Replace & with and
  ## Remove any special characters including spaces
  pudmed_race_matched <- pubmed_race_matched %>% 
    mutate(journal_match = str_to_lower(journal),
           journal_match = str_remove(journal_match, "[.]"),
           journal_match = str_remove(journal_match, "the"),
           journal_match = str_extract(journal_match, "[^:]*"),
           journal_match = str_extract(journal_match, "[^(]+"),
           journal_match = str_extract(journal_match, "[^=]+"),
           journal_match = str_replace(journal_match, "&amp;", "and"),
           journal_match = str_remove_all(journal_match, "[^[:alnum:]]"))
  
  # Match with journal list
  pubmed_matched <- left_join(pudmed_race_matched, sub_journals, by = "journal_match")
  
  # Filter to publications by authors in Australia, include NA as we are not sure where they are from.
  pubmed_final <- pubmed_matched %>% 
    filter(country == "australia" | country == "new zealand" | is.na(country))
  
  # Convert variables to right format
  # Remove the journal matching variable
  pubmed_final <- pubmed_final %>% 
    select(-journal_match) %>%
    rename(journal_h_index = h_index) %>% 
    mutate(pmid = as.character(pmid),
           across(c(order, race, categories), as.factor))
  
  # Save the output as a csv
  data_version <- list.files("data/") %>% 
    str_extract("pubmed_cleaned") %>% 
    na.omit() %>% 
    length()
  
  data_version <- data_version + 1
  
  readr::write_csv(pubmed_final, glue("data/pubmed_cleaned_v{data_version}.csv"))
  return(glue("data/pubmed_cleaned_v{data_version}.csv"))
  
}
