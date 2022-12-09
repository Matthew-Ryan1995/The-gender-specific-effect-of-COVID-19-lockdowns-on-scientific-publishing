#' create_overall_order_state_plot
#' 
#' Creates a plot with date on the x-axis, counts on the y-axis, and coloured by gender.  It is faceted by order and state
#'
#' @param df - The data frame
create_overall_order_state_plot <- function(df, return_plot = TRUE, end_year){
  overall_order_state_plot <- df %>% 
    filter(year > 2018, year <=end_year,
           publishing_states != "NA") %>% 
    select(day, date, gender, order, publishing_states) %>% 
    count(date, order, publishing_states, gender) %>% 
    group_by(date, order) %>% 
    ggplot(aes(x = date, y = n, colour = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_grid(publishing_states~order) +
    labs(x = NULL,
         y = "Number of publications") +
    theme_bw() +
    ggtitle("PubMed Publications across Australia by state and publishing order.")
  
  ggsave("figs/overall_order_state_plot.png")
  
  if(return_plot){
    return(overall_order_state_plot)
  }else{
    return("figs/overall_order_state_plot.png")
  }
}
