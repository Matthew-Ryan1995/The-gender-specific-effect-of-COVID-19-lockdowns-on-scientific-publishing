generate_journal_state_covid_plot <- function(df, tiff = FALSE){
  require(tidyverse, quietly = TRUE)
  require(patchwork, quietly = TRUE)
  
  
  p1 <- df %>% 
    filter(date >= lubridate::as_date("2020-03-01"), date <= lubridate::as_date("2020-12-01"),
           publishing_states != "NA",
           covid) %>% 
    drop_na(gender) %>% 
    group_by(date, publishing_states, gender) %>% 
    summarise(n = sum(n), .groups = "drop") %>% 
    group_by(publishing_states) %>% 
    mutate(p = n/sum(n),
           order = "All authors") %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    facet_wrap(~publishing_states, nrow = 1) +
    labs(x = NULL,
         y = NULL,
         colour = "Sex",
         shape = "Sex",
         title = "All authors") +
    theme_classic() +
    # ggtitle("Total output of all authors") +
    harrypotter::scale_colour_hp_d("Ravenclaw", limits = c("male", "female")) +
    scale_shape(limits = c("male", "female")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) + ####LEGEND POS
    theme(legend.position = c(0.15, 0.25),
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "vertical",
          panel.spacing.x = unit(1, "lines")) +
    guides(color = guide_legend(reverse = TRUE, title.position = "left"),
           shape = guide_legend(reverse = TRUE, title.position = "left")) +
    scale_x_date(breaks = c(
                           lubridate::ymd("2020-03-01"),
                           lubridate::ymd("2020-06-01"),
                           lubridate::ymd("2020-09-01"),
                           lubridate::ymd("2020-12-01")),
                limits = c(lubridate::ymd("2020-03-01"),
                           lubridate::ymd("2020-12-01")),
                date_labels = "%b") 
  # p2 <- df %>% 
  #   filter(date >= lubridate::as_date("2020-03-01"), date <= lubridate::as_date("2020-12-01"),
  #          publishing_states != "NA",
  #          order == "First") %>% 
  #   drop_na(gender) %>% 
  #   group_by(publishing_states, gender, order) %>% 
  #   summarise(n = sum(n), .groups = "drop") %>% 
  #   group_by(order, publishing_states) %>% 
  #   mutate(p = n/sum(n),
  #          order = str_c(order, " authors")) %>% 
  #   ggplot(aes(x = publishing_states, y = p, fill = gender)) +
  #   geom_col(colour = "black") +
  #   geom_segment(aes(y=0, yend=1, x=-Inf, xend=-Inf)) +
  #   scale_y_continuous(labels = scales::label_percent())+
  #   facet_wrap(~order, nrow = 2, scales = "free") +
  #   labs(y = "Percentage of authors",
  #        x = NULL,
  #        fill = "Sex") +
  #   theme_classic() +
  #   ggtitle("") +
  #   harrypotter::scale_fill_hp_d("Ravenclaw", limits = c("male", "female")) +
  #   theme(strip.background =element_rect(fill="gray70"),
  #         strip.text = element_text(face = "bold", size = 14),
  #         axis.line.y = element_blank(),
  #         legend.position = "none") 
  
  p2 <- df %>% 
    filter(date >= lubridate::as_date("2020-03-01"), date <= lubridate::as_date("2020-12-01"),
           publishing_states != "NA",
           order == "First",
           covid) %>% 
    drop_na(gender) %>% 
    group_by(date, publishing_states, gender) %>% 
    summarise(n = sum(n), .groups = "drop") %>% 
    group_by(publishing_states) %>% 
    mutate(p = n/sum(n)) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~publishing_states, nrow = 1) +
    labs(x = NULL,
         y = "Number of publications",
         colour = "Sex",
         shape = "Sex",
         title = "First authors") +
    theme_classic() +
    # ggtitle("Total output of all authors") +
    harrypotter::scale_colour_hp_d("Ravenclaw", limits = c("male", "female")) +
    scale_shape(limits = c("male", "female")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          panel.spacing.x = unit(1, "lines")) +
    scale_x_date(breaks = c(
      lubridate::ymd("2020-03-01"),
      lubridate::ymd("2020-06-01"),
      lubridate::ymd("2020-09-01"),
      lubridate::ymd("2020-12-01")),
      limits = c(lubridate::ymd("2020-03-01"),
                 lubridate::ymd("2020-12-01")),
      date_labels = "%b") 
  
  # p3 <- df %>% 
  #   filter(date >= lubridate::as_date("2020-03-01"), date <= lubridate::as_date("2020-12-01"),
  #          publishing_states != "NA",
  #          order == "Last") %>% 
  #   drop_na(gender) %>% 
  #   group_by(publishing_states, gender, order) %>% 
  #   summarise(n = sum(n), .groups = "drop") %>% 
  #   group_by(order, publishing_states) %>% 
  #   mutate(p = n/sum(n),
  #          order = str_c(order, " authors")) %>% 
  #   ggplot(aes(x = publishing_states, y = p, fill = gender)) +
  #   geom_col(colour = "black") +
  #   geom_segment(aes(y=0, yend=1, x=-Inf, xend=-Inf)) +
  #   scale_y_continuous(labels = scales::label_percent())+
  #   facet_wrap(~order, nrow = 2, scales = "free") +
  #   labs(y = NULL,
  #        x = NULL,
  #        fill = "Sex") +
  #   theme_classic() +
  #   ggtitle("") +
  #   harrypotter::scale_fill_hp_d("Ravenclaw", limits = c("male", "female")) +
  #   theme(strip.background =element_rect(fill="gray70"),
  #         strip.text = element_text(face = "bold", size = 14),
  #         axis.line.y = element_blank(),
  #         legend.position = "bottom") 
  
  p3 <- df %>% 
    filter(date >= lubridate::as_date("2020-03-01"), date <= lubridate::as_date("2020-12-01"),
           publishing_states != "NA",
           order == "Last",
           covid) %>% 
    drop_na(gender) %>% 
    group_by(date, publishing_states, gender) %>% 
    summarise(n = sum(n), .groups = "drop") %>% 
    group_by(publishing_states) %>% 
    mutate(p = n/sum(n)) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~publishing_states, nrow = 1) +
    labs(x = NULL,
         y = NULL,
         colour = "Sex",
         shape = "Sex",
         title = "Last authors") +
    theme_classic() +
    # ggtitle("Total output of all authors") +
    harrypotter::scale_colour_hp_d("Ravenclaw", limits = c("male", "female")) +
    scale_shape(limits = c("male", "female")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          panel.spacing.x = unit(1, "lines")) +
    scale_x_date(breaks = c(
      lubridate::ymd("2020-03-01"),
      lubridate::ymd("2020-06-01"),
      lubridate::ymd("2020-09-01"),
      lubridate::ymd("2020-12-01")),
      limits = c(lubridate::ymd("2020-03-01"),
                 lubridate::ymd("2020-12-01")),
      date_labels = "%b") 
  
  
  p1/p2/p3 
  
  
  ggsave("figs/JOURNAL_state_covid_plot.png")
  if(tiff){
    ggsave("figs/JOURNAL_state__covid_plot.tiff")
  }
  
  return("figs/JOURNAL_state_covid_plot.png")
}