get_race <- function(pubmed_df){
  require(predictrace, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  
  race_test <- predict_race(pubmed_df$lastname, probability = FALSE) %>% 
    select(lastname = name, race = likely_race) %>% 
    distinct(lastname, .keep_all = TRUE)
  
  # Join the race matched data to the gender matched data
  pubmed_race_matched <- left_join(pubmed_df, race_test, by = "lastname")
  
  return(pubmed_race_matched)
}