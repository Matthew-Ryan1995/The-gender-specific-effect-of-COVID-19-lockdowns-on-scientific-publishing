#' create_model_data
#' 
#' This will create a summarised count dataframe for modelling.
#'
#' @param df - The data frame
#' @param year_filter - filter for years > than this year
#'
#' @return
#' @export
#'
#' @examples
create_model_data <- function(df, year_filter = 2018){
  
  model_data <- df %>% 
    filter(year > 2018) %>% 
    count(date, order, covid, paper_length, publishing_states, gender)
  
  return(model_data)
}