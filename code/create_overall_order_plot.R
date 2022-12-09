#' create_overall_order_plot
#' 
#' Creates a plot with date on the x-axis, counts on the y-axis, and coloured by gender.  It is faceted by order.
#'
#' @param df - The data frame
create_overall_order_plot <- function(df, return_plot = TRUE, title = "", end_year){
  overall_order_plot <- df %>% 
    filter(year > 2018, year <=end_year) %>% 
    select(day, date, gender, order) %>% 
    count(date, order, gender) %>% 
    group_by(date, order) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~order, nrow = 2) +
    labs(x = NULL,
         y = "Number of publications") +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(title)
  
  ggsave("figs/overall_order_plot.png")
  
  if(return_plot){
    return(overall_order_plot)
  }else{
    return("figs/overall_order_plot.png")
  }
}
