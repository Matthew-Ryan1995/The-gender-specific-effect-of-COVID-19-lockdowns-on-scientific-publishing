#' create_preprocess_data
#' 
#' This will create some new variables, namely: paper length, order, publishing state, and covid papers.
#'
#' @param df - The data frame
#'
#' @return
#' @export
#'
#' @examples
create_preprocess_data <- function(df, end_year){
  ## Filter to Australia
  df <- df %>% 
    filter(country == "australia",
           year >= 2018)
  # These papers have been defined as "long" and "short"
  # Note that these papers are only valid for years 2019, 2020, and 2021
  long_papers <- df %>% 
    filter(year>2018, year <=end_year) %>% 
    count(pub_type, sort = TRUE) %>% 
    filter(!str_detect(pub_type, "Review|Comment|Letter|Editorial|Case|Meta"))  %>% 
    pull(pub_type)
  short_papers <- df %>% 
    filter(year>2018, year <=end_year) %>% 
    count(pub_type, sort = TRUE) %>% 
    filter(str_detect(pub_type, "Review|Comment|Letter|Editorial|Case|Meta"))  %>% 
    pull(pub_type)
  
  # Create a new variable called paper_length
  df <- df %>% 
    mutate(paper_length = case_when(pub_type %in% long_papers ~ "Long",
                                    pub_type %in% short_papers ~ "Short",
                                    TRUE ~ "No data")) 
  
  # Make single authors last authors.  We can regather this information using the num_authors variable.
  df <- df %>% 
    mutate(order = ifelse(order == "Single", "Last", order)) 
  
  # Concatenate states to make data more comparable.  Keeping NSW, VIC, and QLD, then combining the rest
  df <- df %>% 
    mutate(publishing_states = case_when(state == "New South Wales" ~ "NSW",
                                         state == "Victoria" ~ "VIC",
                                         state == "Queensland" ~ "QLD",
                                         is.na(state) ~ "NA",
                                         TRUE ~ "Other"),
           publishing_states = factor(publishing_states, c("NSW", "VIC", "QLD", "Other", "NA")))
  
  # Create a "covid" variable by searching for covid or sars-cov2 in the title or keywords.
  df <- df %>% 
    mutate(covid = case_when(str_detect(str_to_lower(title), "covid|sars-cov2") ~ TRUE,
                             str_detect(str_to_lower(keywords), "covid|sars-cov2") ~ TRUE,
                             TRUE ~ FALSE)) 
  
  # Create Date variable.
  df <- df %>% 
    mutate(date = str_c(year, month, "01", sep ="-"),
           date = lubridate::as_date(date))
  
  return(df)
}