get_country <- function(pubmed_df){
  require(tidyverse, quietly = TRUE)
  
  countries <- unique(gapminder::gapminder$country) %>% 
    as.character()%>% 
    str_extract("[^,]*")
  countries[143] <- "USA"
  countries[144] <- "UK"
  countries[145] <- "Estonia"
  countries[146] <- "Qatar"
  countries[147] <- "Fiji"
  countries[148] <- "Brasil"
  
  # Convert to a logical string statement
  countries <- str_to_lower(str_c(countries, collapse = "|") )
  
  pubmed_country_filter <- pubmed_df %>% 
    mutate(country = str_extract(str_to_lower(address), 
                                 countries)) %>% 
    filter(country == "australia" | country == "new zealand" | is.na(country))
  
  return(pubmed_country_filter)
}