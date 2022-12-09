generate_journal_covid_inset_plot <- function(df, tiff = FALSE, end_year){
  require(tidyverse, quietly = TRUE)
  require(patchwork, quietly = TRUE)
  require(egg, quietly = TRUE)
  # require(lemon, quietly = TRUE)s
  
  df <- df %>% 
    mutate(gender = ifelse(gender == "female", "women", "men"))
  
  ## plot of COVID papers
  covid_plot_covid_only <- df %>% 
    filter(year > 2018, year <end_year, !(year == 2019 & covid), covid) %>% 
    drop_na(gender) %>% 
    mutate(covid = ifelse(covid, "COVID research", "Non-COVID research")) %>% 
    select(day, date, gender, covid) %>% 
    count(date, covid, gender) %>% 
    group_by(date, covid) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~covid, nrow = 2) +
    labs(x = NULL,
         y = "Number of publications",
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    ggtitle("") +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14))
  
  
  ## NON-covid plot
  covid_plot_noncovid_only <- df %>% 
    filter(year > 2018, year <end_year, !(year == 2019 & covid), !covid) %>% 
    drop_na(gender) %>% 
    mutate(covid = ifelse(covid, "COVID research", "Non-COVID research")) %>% 
    select(day, date, gender, covid) %>% 
    count(date, covid, gender) %>% 
    group_by(date, covid) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~covid, nrow = 2) +
    labs(x = NULL,
         y = "Number of publications",
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    ggtitle("") +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14))
  
  
  # Proportion of covid papers by Gender
  covid_numbers <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), covid) %>% 
    count(gender, covid) %>% 
    mutate(p = round(n/sum(n), 3)*100,
           n = 800,
           p = str_c(p, "%"))
  
  
  ## Proportion non-covid by Gender
  noncovid_numbers <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    mutate(p = round(n/sum(n), 3)*100,
           n = 20000,
           p = str_c(p, "%"))
  
  
  
  # Proportion of 2020 COVID counts
  covid_2020_counts <- df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), covid) %>% 
    count(gender, covid) %>% 
    ggplot(aes(x = gender, y = n, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
    geom_text(data = covid_numbers, aes(label = p), 
              size = 3,
              colour = "white") + # PLAYING WITH SIZE
    labs(x = NULL, y = NULL) +
    ggtitle("2020 Authorship") +
    theme_classic() +
    theme(plot.title = element_text(size = 8),
          axis.line.y = element_blank()) +
    geom_segment(aes(y=0, yend=2001, x=-Inf, xend=-Inf)) +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women"))
  
  # Proportion of 2021 COVID counts
  # Proportion of covid papers by Gender
  covid_numbers <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2021, !(year == 2019 & covid), covid) %>% 
    count(gender, covid) %>% 
    mutate(p = round(n/sum(n), 3)*100,
           n = 800,
           p = str_c(p, "%"))
  
  covid_2021_counts <- df %>% 
    drop_na(gender) %>% 
    filter(year == 2021, !(year == 2019 & covid), covid) %>% 
    count(gender, covid) %>% 
    ggplot(aes(x = gender, y = n, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
    geom_text(data = covid_numbers, aes(label = p), 
              size = 3,
              colour = "white") + # PLAYING WITH SIZE
    labs(x = NULL, y = NULL) +
    ggtitle("2021 Authorship") +
    theme_classic() +
    theme(plot.title = element_text(size = 8),
          axis.line.y = element_blank()) +
    geom_segment(aes(y=0, yend=1501, x=-Inf, xend=-Inf)) +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women"))
  
  
  # Proportion of 2020 non-COVID counts
  count_max <- df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    slice_max(n) %>% 
    pull(n)
  noncovid_2020_counts <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    ggplot(aes(x = gender, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
    geom_text(data = noncovid_numbers, aes(label = p),
              size = 3,
              colour = "white") + #PLAYING WITH SIZE
    labs(x = NULL, y = NULL) +
    ggtitle("2020 Authorship ('000s)") +
    theme_classic() + 
    theme(plot.title = element_text(size = 8),
          axis.line.y = element_blank()) +
    geom_segment(aes(y=0, yend=30, x=-Inf, xend=-Inf)) +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  # Proportion of 2021 non-COVID counts
  ## Proportion non-covid by Gender
  noncovid_numbers <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2021, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    mutate(p = round(n/sum(n), 3)*100,
           n = 20000,
           p = str_c(p, "%"))
  
  count_max <- df %>% 
    drop_na(gender) %>% 
    filter(year == 2021, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    slice_max(n) %>% 
    pull(n)
  noncovid_2021_counts <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2021, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    ggplot(aes(x = gender, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
    geom_text(data = noncovid_numbers, aes(label = p),
              size = 3,
              colour = "white") + #PLAYING WITH SIZE
    labs(x = NULL, y = NULL) +
    ggtitle("2021 Authorship ('000s)") +
    theme_classic() + 
    theme(plot.title = element_text(size = 8),
          axis.line.y = element_blank()) +
    geom_segment(aes(y=0, yend=30, x=-Inf, xend=-Inf)) +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  
  p1 <- covid_plot_covid_only + 
    # xlim(c(lubridate::ymd("2019-01-01"), lubridate::ymd("2020-12-01")))+
    scale_x_date(breaks = c(lubridate::ymd("2019-01-01"),
                            lubridate::ymd("2019-07-01"),
                            lubridate::ymd("2020-01-01"),
                            lubridate::ymd("2020-07-01"),
                            lubridate::ymd("2021-01-01"),
                            lubridate::ymd("2021-07-01"),
                            lubridate::ymd("2021-12-01")),
                 limits = c(lubridate::ymd("2019-01-01"),
                            lubridate::ymd("2021-12-01")),
                 date_labels = "%b %Y")+
    annotation_custom(
      ggplotGrob(covid_2020_counts), 
      xmin = lubridate::ymd("2019-02-01"), xmax = lubridate::ymd("2019-11-01"), ymin = 110, ymax = 210
    ) +
    annotation_custom(
      ggplotGrob(covid_2021_counts), 
      xmin = lubridate::ymd("2019-02-01"), xmax = lubridate::ymd("2019-11-01"), ymin = 0, ymax = 100
    ) +
    theme(legend.position = c(0.85, 0.25),
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "vertical") +
    guides(color = guide_legend(reverse = TRUE, title.position = "left"),
           shape = guide_legend(reverse = TRUE, title.position = "left")) 
  p2 <- covid_plot_noncovid_only + 
    # xlim(c(lubridate::ymd("2019-01-01"), lubridate::ymd("2020-12-01")))+
    scale_x_date(breaks = c(lubridate::ymd("2019-01-01"),
                            lubridate::ymd("2019-07-01"),
                            lubridate::ymd("2020-01-01"),
                            lubridate::ymd("2020-07-01"),
                            lubridate::ymd("2021-01-01"),
                            lubridate::ymd("2021-07-01"),
                            lubridate::ymd("2021-12-01")),
                 limits = c(lubridate::ymd("2019-01-01"),
                            lubridate::ymd("2021-12-01")),
                 date_labels = "%b %Y") + 
    ylim(c(140, 3000))+
    annotation_custom(
      ggplotGrob(noncovid_2020_counts), 
      xmin = lubridate::ymd("2019-02-01"), xmax = lubridate::ymd("2019-11-01"), ymin = -10, ymax = 1800
    ) +
    annotation_custom(
      ggplotGrob(noncovid_2021_counts), 
      xmin = lubridate::ymd("2020-02-01"), xmax = lubridate::ymd("2020-11-01"), ymin = -10, ymax = 1800
    ) +
    theme(legend.position = "none")
  
  
  p1 / p2
  
  ggsave("figs/JOURNAL_index_covid_plot.png")
  
  if(tiff){
    ggsave("figs/JOURNAL_index_covid_plot.tiff")
  }
  
  return("figs/JOURNAL_index_covid_plot.png")
}
generate_journal_covid_inset_plot_2020 <- function(df, tiff = FALSE, end_year){
  require(tidyverse, quietly = TRUE)
  require(patchwork, quietly = TRUE)
  require(egg, quietly = TRUE)
  # require(lemon, quietly = TRUE)s
  
  df <- df %>% 
    mutate(gender = ifelse(gender == "female", "women", "men"),
           gender = factor(gender, c("women", "men")))
  
  ## plot of COVID papers
  covid_plot_covid_only <- df %>% 
    filter(year > 2018, year <end_year, !(year == 2019 & covid), covid) %>% 
    drop_na(gender) %>% 
    mutate(covid = ifelse(covid, "COVID research", "Non-COVID research")) %>% 
    select(day, date, gender, covid) %>% 
    count(date, covid, gender) %>% 
    group_by(date, covid) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    # geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~covid, nrow = 2) +
    labs(x = NULL,
         y = "Number of publications",
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    ggtitle("") +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14))
  
  
  ## NON-covid plot
  covid_plot_noncovid_only <- df %>% 
    filter(year > 2018, year <end_year, !(year == 2019 & covid), !covid) %>% 
    drop_na(gender) %>% 
    mutate(covid = ifelse(covid, "COVID research", "Non-COVID research")) %>% 
    select(day, date, gender, covid) %>% 
    count(date, covid, gender) %>% 
    group_by(date, covid) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    # geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~covid, nrow = 2) +
    labs(x = NULL,
         y = "Number of publications",
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    ggtitle("") +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14))
  
  
  # Proportion of covid papers by Gender
  covid_numbers <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), covid) %>% 
    count(gender, covid) %>% 
    mutate(p = round(n/sum(n), 3)*100,
           n = 800,
           p = str_c(p, "%"))
  
  
  ## Proportion non-covid by Gender
  noncovid_numbers <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    mutate(p = round(n/sum(n), 3)*100,
           n = 20000,
           p = str_c(p, "%"))
  
  
  
  # Proportion of 2020 COVID counts
  covid_2020_counts <- df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), covid) %>% 
    count(gender, covid) %>% 
    ggplot(aes(x = gender, y = n, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
    geom_text(data = covid_numbers, aes(label = p), 
              size = 3,
              colour = "white") + # PLAYING WITH SIZE
    labs(x = NULL, y = NULL) +
    ggtitle("2020 Authorship") +
    theme_classic() +
    theme(plot.title = element_text(size = 8),
          axis.line.y = element_blank()) +
    geom_segment(aes(y=0, yend=2001, x=-Inf, xend=-Inf)) +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women"))
  
  # # Proportion of 2021 COVID counts
  # # Proportion of covid papers by Gender
  # covid_numbers <- 
  #   df %>% 
  #   drop_na(gender) %>% 
  #   filter(year == 2021, !(year == 2019 & covid), covid) %>% 
  #   count(gender, covid) %>% 
  #   mutate(p = round(n/sum(n), 3)*100,
  #          n = 800,
  #          p = str_c(p, "%"))
  # 
  # covid_2021_counts <- df %>% 
  #   drop_na(gender) %>% 
  #   filter(year == 2021, !(year == 2019 & covid), covid) %>% 
  #   count(gender, covid) %>% 
  #   ggplot(aes(x = gender, y = n, fill = gender)) +
  #   geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
  #   geom_text(data = covid_numbers, aes(label = p), 
  #             size = 3,
  #             colour = "white") + # PLAYING WITH SIZE
  #   labs(x = NULL, y = NULL) +
  #   ggtitle("2021 Authorship") +
  #   theme_classic() +
  #   theme(plot.title = element_text(size = 8),
  #         axis.line.y = element_blank()) +
  #   geom_segment(aes(y=0, yend=1501, x=-Inf, xend=-Inf)) +
  #   harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women"))
  
  
  # Proportion of 2020 non-COVID counts
  count_max <- df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    slice_max(n) %>% 
    pull(n)
  noncovid_2020_counts <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year == 2020, !(year == 2019 & covid), !covid) %>% 
    count(gender, covid) %>% 
    ggplot(aes(x = gender, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
    geom_text(data = noncovid_numbers, aes(label = p),
              size = 3,
              colour = "white") + #PLAYING WITH SIZE
    labs(x = NULL, y = NULL) +
    ggtitle("2020 Authorship ('000s)") +
    theme_classic() + 
    theme(plot.title = element_text(size = 8),
          axis.line.y = element_blank()) +
    geom_segment(aes(y=0, yend=35, x=-Inf, xend=-Inf)) +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  # # Proportion of 2021 non-COVID counts
  # ## Proportion non-covid by Gender
  # noncovid_numbers <- 
  #   df %>% 
  #   drop_na(gender) %>% 
  #   filter(year == 2021, !(year == 2019 & covid), !covid) %>% 
  #   count(gender, covid) %>% 
  #   mutate(p = round(n/sum(n), 3)*100,
  #          n = 20000,
  #          p = str_c(p, "%"))
  # 
  # count_max <- df %>% 
  #   drop_na(gender) %>% 
  #   filter(year == 2021, !(year == 2019 & covid), !covid) %>% 
  #   count(gender, covid) %>% 
  #   slice_max(n) %>% 
  #   pull(n)
  # noncovid_2021_counts <- 
  #   df %>% 
  #   drop_na(gender) %>% 
  #   filter(year == 2021, !(year == 2019 & covid), !covid) %>% 
  #   count(gender, covid) %>% 
  #   ggplot(aes(x = gender, y = n/1000, fill = gender)) +
  #   geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
  #   geom_text(data = noncovid_numbers, aes(label = p),
  #             size = 3,
  #             colour = "white") + #PLAYING WITH SIZE
  #   labs(x = NULL, y = NULL) +
  #   ggtitle("2021 Authorship ('000s)") +
  #   theme_classic() + 
  #   theme(plot.title = element_text(size = 8),
  #         axis.line.y = element_blank()) +
  #   geom_segment(aes(y=0, yend=30, x=-Inf, xend=-Inf)) +
  #   harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
  #   theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
  #         plot.background = element_rect(fill = "transparent", color = NA)) 
  # 
  
  p1 <- covid_plot_covid_only + 
    # xlim(c(lubridate::ymd("2019-01-01"), lubridate::ymd("2020-12-01")))+
    scale_x_date(breaks = c(lubridate::ymd("2019-01-01"),
                            lubridate::ymd("2019-07-01"),
                            lubridate::ymd("2020-01-01"),
                            lubridate::ymd("2020-07-01"),
                            lubridate::ymd("2020-12-01")
                            # lubridate::ymd("2021-01-01"),
                            # lubridate::ymd("2021-07-01"),
                            # lubridate::ymd("2021-12-01")
                            ),
                 limits = c(lubridate::ymd("2019-01-01"),
                            lubridate::ymd("2020-12-01")),
                 date_labels = "%b %Y")+
    annotation_custom(
      ggplotGrob(covid_2020_counts), 
      xmin = lubridate::ymd("2019-02-01"), xmax = lubridate::ymd("2019-11-01"), ymin = 50, ymax = 200
    ) +
    # annotation_custom(
    #   ggplotGrob(covid_2021_counts), 
    #   xmin = lubridate::ymd("2019-02-01"), xmax = lubridate::ymd("2019-11-01"), ymin = 0, ymax = 100
    # ) +
    theme(legend.position = c(0.85, 0.25),
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "vertical") +
    guides(color = guide_legend(reverse = TRUE, title.position = "left"),
           shape = guide_legend(reverse = TRUE, title.position = "left")) 
  p2 <- covid_plot_noncovid_only + 
    # xlim(c(lubridate::ymd("2019-01-01"), lubridate::ymd("2020-12-01")))+
    scale_x_date(breaks = c(lubridate::ymd("2019-01-01"),
                            lubridate::ymd("2019-07-01"),
                            lubridate::ymd("2020-01-01"),
                            lubridate::ymd("2020-07-01"),
                            lubridate::ymd("2020-12-01")
                            # lubridate::ymd("2021-01-01"),
                            # lubridate::ymd("2021-07-01"),
                            # lubridate::ymd("2021-12-01")
                            ),
                 limits = c(lubridate::ymd("2019-01-01"),
                            lubridate::ymd("2020-12-01")),
                 date_labels = "%b %Y") + 
    ylim(c(140, 3200))+
    annotation_custom(
      ggplotGrob(noncovid_2020_counts), 
      xmin = lubridate::ymd("2019-02-01"), xmax = lubridate::ymd("2019-11-01"), ymin = -10, ymax = 1800
    ) +
    # annotation_custom(
    #   ggplotGrob(noncovid_2021_counts), 
    #   xmin = lubridate::ymd("2020-02-01"), xmax = lubridate::ymd("2020-11-01"), ymin = -10, ymax = 1800
    # ) +
    theme(legend.position = "none")
  
  
  p1 / p2
  
  ggsave("figs/JOURNAL_index_covid_plot_2020.png")
  
  if(tiff){
    ggsave("figs/JOURNAL_index_covid_plot_2020.tiff") 
  }
  print("CHANGE")
  return("figs/JOURNAL_index_covid_plot_2020.png")
}