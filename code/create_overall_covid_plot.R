#' create_overall_covid_plot
#' 
#' Creates a plot with date on the x-axis, counts on the y-axis, and coloured by gender.  It is faceted by covid.
#'
#' @param df - The data frame
create_overall_covid_plot <- function(df, return_plot = TRUE, covid.only = TRUE, title = "", end_year){
  if(covid.only){
    overall_covid_plot <- df %>% 
      filter(year > 2018, year <=end_year, covid) %>% 
      mutate(covid = ifelse(covid, "COVID research", "Non-COVID research")) %>% 
      select(day, date, gender, covid) %>% 
      count(date, covid, gender) %>% 
      group_by(date, covid) %>% 
      ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
      geom_point()+
      geom_line() +
      geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
      geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
      facet_wrap(~covid, scales = "free_y", nrow = 2) +
      labs(x = NULL,
           y = "Number of publications") +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(title)
  }else{
    overall_covid_plot <- df %>% 
      filter(year > 2018, year <=end_year) %>% 
      mutate(covid = ifelse(covid, "COVID research", "Non-COVID research")) %>% 
      select(day, date, gender, covid) %>% 
      count(date, covid, gender) %>% 
      group_by(date, covid) %>% 
      ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
      geom_point()+
      geom_line() +
      geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
      geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
      facet_wrap(~covid, scales = "free_y", nrow = 2) +
      labs(x = NULL,
           y = "Number of publications") +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(title)
  }
  
  ggsave("figs/overall_covid_plot.png")
  
  if(return_plot){
    return(overall_covid_plot)
  }else{
    return("figs/overall_covid_plot.png")
  }
  
}
