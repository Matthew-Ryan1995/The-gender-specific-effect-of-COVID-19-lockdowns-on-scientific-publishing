get_institute_and_state <- function(pubmed_df){
  require(tidyverse, quietly = TRUE)
  require(easyPubMed, quietly = TRUE)
  
  institute_names <- read_csv("raw_data/providers_19112020a.csv", col_types = cols()) %>% 
    select(ProviderName, Category, Address)
  
  aus_all <- read_csv("raw_data/au.csv", col_types = cols())
  
  
  aus_all <- aus_all %>% 
    select(city, admin_name) %>% 
    filter(!(city %in% c("York", "Sheffield", "Warwick", "Weston",
                         "Monash", "Griffith", "Stirling", "Ingham",
                         "Alexandra", "Richmond", "Churchill", "Wellington"))) %>% 
    bind_rows(
      tibble(
        city = c("Parkville",
                 "Clayton",
                 "Bedford Park",
                 "Pymble",
                 "Wesley Bowden",
                 "Woolloongabba",
                 "Nedlands",
                 "Subiaco",
                 "Ermington",
                 "Lyndon",
                 "Gongolgon",
                 "Elizabeth Vale",
                 "Heidelberg"),
        admin_name = c("Victoria",
                       "Victoria",
                       "South Australia",
                       "New South Wales",
                       "South Australia",
                       "Queensland",
                       "Western Australia",
                       "Western Australia",
                       "New South Wales",
                       "Western Australia",
                       "New South Wales",
                       "South Australia",
                       "Victoria")
      )
    )
  # Create search data for each city and state
  aus_cities <- aus_all$city
  aus_states_abbrv <- c("SA", "Vic", "VIC", "NSW", "ACT", "WA", "TAS", "QLD", "NT", "Qld")
  aus_state <- c("South Australia", "Victoria", "Victoria", "New South Wales", 
                 "Australian Capital Territory", "Western Australia", "Tasmania",
                 "Queensland", "Northern Territory", "Queensland")
  
  aus_states <- tibble(state = aus_state,
                       abbr = aus_states_abbrv)
  
  # Change to logical string searches
  aus_cities <- str_c((aus_cities), collapse = "|")
  aus_states_abbrv <- str_c((aus_states_abbrv), collapse = "|")
  aus_state <- str_c((aus_state), collapse = "|")
  
  # Clean institute names
  institute_names_list <- institute_names %>% 
    # filter(Category == "Australian University") %>% # Filter to get universities only
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
    mutate(
      Address = str_sub(Address, start = -8L), # Get state and post code
      state_abbrv = str_extract(Address, aus_states_abbrv)
    ) %>% 
    select(ProviderName, state_abbrv) %>% 
    bind_rows( # Some missed inputs
      tibble(
        ProviderName = c("Australian Institute of Sport",
                         "Australian College of Physical Education",
                         "Complete Sports Care",
                         "Australian Centre for Electromagnetic Bioeffects Research",
                         "Lady Cilento Children's Hospital",
                         "King Edward Memorial Hospital",
                         "Football Federation Australia",
                         "Royal North Shore Hospital",
                         "Queen Elizabeth Hospital"),
        state_abbrv = c("ACT",
                        "NSW",
                        "VIC",
                        "NSW",
                        "QLD",
                        "WA",
                        "NSW",
                        "NSW",
                        "SA")
      )
    ) %>% 
    drop_na()
  
  institute_state_list <- institute_names_list %>% 
    pull(state_abbrv)
  
  institute_names_list <- institute_names_list %>% 
    pull(ProviderName)
  # Add Other names
  institute_names_list <- c(institute_names_list,
                            "UniSA", "UNSW", "RMIT", 
                            "Sunshine Coast University", "Gold Coast University",
                            "CSIRO")
  institute_state_list <- c(institute_state_list,
                            "SA", "NSW", "VIC", "QLD", "QLD", "NA")
  # institute_names_list[41] <- "UniSA"
  # institute_names_list[42] <- "UNSW"
  # institute_names_list[43] <- "RMIT"
  # institute_names_list[44] <- "Sunshine Coast University"
  # institute_names_list[45] <- "Gold Coast University"
  # institute_names_list[46] <- "CSIRO"
  
  institute_list <- tibble(name = institute_names_list,
                           match_name = institute_names_list %>% 
                             str_to_lower() %>% 
                             str_remove_all(str_c(PubMed_stopwords, collapse = "|")) %>% 
                             str_remove_all("[^[:alnum:]]"))
  
  institute_names_list <- institute_list$match_name %>%
    str_c(collapse = "|")
  
  
  
  
  # aus_all <- read_csv("raw_data/au.csv", col_types = cols())
  # 
  # 
  # aus_all <- aus_all %>% 
  #   filter(!(city %in% c("York", "Sheffield", "Warwick", "Weston",
  #                        "Monash", "Griffith", "Stirling", "Ingham",
  #                        "Alexandra", "Richmond", "Churchill", "Wellington")))
  # # Create search data for each city and state
  # aus_cities <- aus_all$city
  # aus_states_abbrv <- c("SA", "Vic", "VIC", "NSW", "ACT", "WA", "TAS", "QLD", "NT", "Qld")
  # aus_state <- c("South Australia", "Victoria", "Victoria", "New South Wales", 
  #                "Australian Capital Territory", "Western Australia", "Tasmania",
  #                "Queensland", "Northern Territory")
  # 
  # aus_states <- tibble(state = aus_state,
  #                      abbr = aus_states_abbrv)
  # 
  # # Change to logical string searches
  # aus_cities <- str_c((aus_cities), collapse = "|")
  # aus_states_abbrv <- str_c((aus_states_abbrv), collapse = "|")
  # aus_state <- str_c((aus_state), collapse = "|")
  
  pubmed_institutes <- pubmed_df %>% 
    mutate(
      institute = address %>% 
        str_to_lower() %>% 
        str_remove_all(str_c(PubMed_stopwords, collapse = "|")) %>% 
        str_remove_all("[^[:alnum:]]") %>% 
        str_extract(institute_names_list),
      institute = map_chr(institute, function(str){
        if(str %in% institute_list$match_name){
          return(institute_list$name[map_lgl(institute_list$match_name,
                                             function(name){
                                               return(str==name)
                                             })])
        }else{
          return(NA)
        }
      }),
      city = str_extract(address, aus_cities),
      state_abr = str_extract(address, aus_states_abbrv),
      state_abr = case_when(is.na(state_abr) & !is.na(institute) ~ map_chr(institute,
                                                                           function(i){
                                                                             if(is.na(i)){
                                                                               return(NA)
                                                                             }
                                                                             institute_state_list[which(institute_list$name == i)]
                                                                           }),
                            TRUE ~ state_abr),
      institute = case_when(institute == "RMIT" ~ "Royal Melbourne Institute of Technology",
                            institute == "UniSA" ~ "University of South Australia",
                            institute == "Sunshine Coast University" ~ "University of the Sunshine Coast",
                            institute == "UNSW" ~ "University of New South Wales",
                            TRUE ~ institute),
      state = str_extract(address, aus_state),
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
  
  return(pubmed_institutes)
}