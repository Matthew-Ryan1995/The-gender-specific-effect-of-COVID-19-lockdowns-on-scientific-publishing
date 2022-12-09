#' create_overall_plot
#' 
#' Creates a plot with date on the x-axis, counts on the y-axis, and coloured by gender.
#'
#' @param df - The data frame
create_overall_plot <- function(df, return_plot = TRUE, title = "", end_year){
  overall_plot <- df %>% 
    filter(year > 2018, year <=end_year) %>% 
    select(day, date, gender) %>% 
    count(date, gender) %>% 
    group_by(date) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    labs(x = NULL,
         y = "Number of publications") +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(title)
  
  ggsave("figs/overall_plot.png")
  
  if(return_plot){
    return(overall_plot)
  }else{
    return("figs/overall_plot.png")
  }
}
create_overall_plot_2020 <- function(df, return_plot = TRUE, title = "", end_year){
  overall_plot <- df %>% 
    filter(year > 2018, year <=end_year) %>% 
    select(day, date, gender) %>% 
    count(date, gender) %>% 
    group_by(date) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    # geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    labs(x = NULL,
         y = "Number of publications") +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(title)
  
  ggsave("figs/2020_overall_plot.png")
  
  if(return_plot){
    return(overall_plot)
  }else{
    return("figs/overall_plot.png")
  }
}
