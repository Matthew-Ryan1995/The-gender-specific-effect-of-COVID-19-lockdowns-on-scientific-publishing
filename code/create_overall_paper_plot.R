#' create_overall_paper_plot
#' 
#' Creates a plot with date on the x-axis, counts on the y-axis, and coloured by gender.  It is faceted by paper_length.
#'
#' @param df - The data frame
create_overall_paper_plot <- function(df, return_plot = TRUE, end_year){
  overall_paper_plot <- df %>% 
    filter(year > 2018, year <=end_year,
           paper_length != "No data") %>% 
    select(day, date, gender, paper_length) %>% 
    count(date, paper_length, gender) %>% 
    group_by(date, paper_length) %>% 
    ggplot(aes(x = date, y = n, colour = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~paper_length, nrow = 2, scales = "free_y") +
    labs(x = NULL,
         y = "Number of publications") +
    theme_bw() +
    ggtitle("PubMed Publications across Australia by paper length")
  
  ggsave("figs/overall_paper_plot.png")
  
  if(return_plot){
    return(overall_paper_plot)
  }else{
    return("figs/overall_paper_plot.png")
  }
}
