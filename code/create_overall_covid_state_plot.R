#' create_overall_covid_state_plot
#' 
#' Creates a plot with date on the x-axis, counts on the y-axis, and coloured by gender.  It is faceted by publishing_states and covid.
#'
#' @param df - The data frame
create_overall_covid_state_plot <- function(df, return_plot = TRUE, end_year){
  overall_covid_state_plot <- df %>% 
    filter(year > 2018, year <=end_year,
           publishing_states != "NA") %>% 
    mutate(covid = ifelse(covid, "COVID research", "Non-COVID research")) %>% 
    select(day, date, gender, publishing_states, covid) %>% 
    count(date, publishing_states, covid, gender) %>% 
    group_by(date, publishing_states) %>%
    ggplot(aes(x = date, y = n, colour = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_grid(covid ~ publishing_states, scales = "free_y") +
    labs(x = NULL,
         y = "Number of publications") +
    theme_bw() +
    ggtitle("PubMed Publications across Australia by COVID papers and publishing state.")
  
  ggsave("figs/overall_covid_state_plot.png", width = 10)
  
  if(return_plot){
    return(overall_covid_state_plot)
  }else{
    return("figs/overall_covid_state_plot.png")
  }
}
