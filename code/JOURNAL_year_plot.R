generate_journal_year_plot <- function(df, tiff = FALSE, end_year){
  require(tidyverse, quietly = TRUE)
  require(patchwork, quietly = TRUE)
  require(egg, quietly = TRUE)
  
  
  df <- df %>% 
    mutate(gender = ifelse(gender == "female", "women", "men"),
           gender = factor(gender, c("women", "men")))
  ## All author counts
  
  all_authors <- df %>% 
    filter(year %in% 2019:end_year) %>% 
    drop_na(gender) %>% 
    count(gender, year) %>% 
    mutate(year = as.factor(year)) %>% 
    group_by(year) %>% 
    mutate(p = round(100*n/sum(n), 1),
           p = str_c(p, "%"),
           n = 15000)
  
  
  ## All author column plot (Has title plot)
  p1_inset <- df %>% 
    filter(year %in% 2019:end_year) %>% 
    drop_na(gender) %>% 
    count(gender, year) %>% 
    mutate(year = as.factor(year)) %>% 
    ggplot(aes(x = year, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") + 
    # geom_text(data = all_authors, aes(label = p), 
    #           colour = "white", position = position_dodge(width = 0.9))+
    geom_segment(aes(y=0, yend=30, x=-Inf, xend=-Inf)) +
    labs(x = NULL,
         y = NULL) +
    theme_classic() +
    # # ggtitle("Number of authorships ('000s)") +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          axis.line.y = element_blank()) +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  ## All author line chart
  p1 <- df %>% 
    filter(year %in% 2019:end_year) %>% 
    drop_na(gender) %>% 
    select(date, gender) %>% 
    count(date, gender) %>% 
    mutate(order = "All authors") %>% 
    group_by(date) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~order, nrow = 2) +
    labs(x = NULL,
         y = NULL,
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    # ggtitle("Total output of all authors") +
    ylim(0, NA) +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14))
  
  
  ## First author counts
  first_author <- df %>% 
    filter(year %in% 2019:end_year, order == "First") %>% 
    drop_na(gender) %>% 
    count(gender, year) %>% 
    mutate(year = as.factor(year)) %>% 
    group_by(year) %>% 
    mutate(p = round(100*n/sum(n), 1),
           p = str_c(p, "%"),
           n = 7500)
  
  
  ## First author column plot (Has y label)
  p2_inset <- df %>% 
    filter(year %in% 2019:end_year, order == "First") %>% 
    drop_na(gender) %>% 
    count(gender, year, order) %>% 
    mutate(year = as.factor(year),
           order = str_c(order, " author")) %>% 
    ggplot(aes(x = year, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") + 
    # geom_text(data = first_author, aes(label = p), 
    #           colour = "white", position = position_dodge(width = 0.9),
    #           size = 2)+
    geom_segment(aes(y=0, yend=15, x=-Inf, xend=-Inf)) +
    labs(x = NULL,
         y = NULL) +
    theme_classic() +
    # # ggtitle("Number of authorships ('000s)") +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          axis.line.y = element_blank()) +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  ## First author line chart
  
  p2 <- df %>% 
    filter(year %in% 2019:end_year,  order == "First") %>% 
    mutate(order = str_c(order," authors")) %>% 
    drop_na(gender) %>% 
    select(date, gender, order) %>% 
    count(date, gender, order) %>% 
    group_by(date) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~order, nrow = 2) +
    labs(x = NULL,
         y = "Number of publications",
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    ylim(0, NA) +
    # ggtitle("Total output of first authors") +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14)) 
  
  
  # Last author counts
  last_author <- df %>% 
    filter(year %in% 2019:end_year, order == "Last") %>% 
    drop_na(gender) %>% 
    count(gender, year) %>% 
    mutate(year = as.factor(year)) %>% 
    group_by(year) %>% 
    mutate(p = round(100*n/sum(n), 1),
           p = str_c(p, "%"),
           n = 7500)
  
  
  ## Last author column plot (Has legend)
  p3_inset <- df %>% 
    filter(year %in% 2019:end_year, order == "Last") %>% 
    drop_na(gender) %>% 
    count(gender, year, order) %>% 
    mutate(year = as.factor(year),
           order = str_c(order, " author")) %>% 
    ggplot(aes(x = year, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", colour = "black") + 
    # geom_text(data = last_author, aes(label = p), 
    #           colour = "white", position = position_dodge(width = 0.9))+
    geom_segment(aes(y=0, yend=20, x=-Inf, xend=-Inf)) +
    labs(x = NULL,
         y = NULL,
         fill = NULL) +
    theme_classic() +
    # # ggtitle("Number of authorships ('000s)") +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          axis.line.y = element_blank(),
          legend.position = "none") +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  ## Last author line chart
  
  p3 <- df %>% 
    filter(year %in% 2019:end_year,  order == "Last") %>% 
    mutate(order = str_c(order," authors")) %>% 
    drop_na(gender) %>% 
    select(date, gender, order) %>% 
    count(date, gender, order) %>% 
    group_by(date) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~order, nrow = 2) +
    labs(x = NULL,
         y = NULL,
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    # ggtitle("Total output of Last authors") +
    ylim(0, NA) +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14))
  
  p1_with_inset <- p1 + 
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
      ggplotGrob(p1_inset), 
      xmin = lubridate::ymd("2020-03-01"), xmax = lubridate::ymd("2020-12-01"), ymin = -200, ymax = 2000
    ) +
    theme(legend.position = c(0.2, 0.25),
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "vertical") +
    guides(color = guide_legend(reverse = TRUE, title.position = "left"),
           shape = guide_legend(reverse = TRUE, title.position = "left")) 
  # theme(legend.position = c(0.25, 0.75),
  #       legend.background = element_rect(fill = "transparent"),
  #       legend.direction = "vertical") +
  # guides(color = guide_legend(reverse = TRUE, title.position = "left"),
  #        shape = guide_legend(reverse = TRUE, title.position = "left")) 
  p2_with_inset <- p2 + 
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
    annotation_custom(
      ggplotGrob(p2_inset), 
      xmin = lubridate::ymd("2020-03-01"), xmax = lubridate::ymd("2020-12-01"), ymin = -125, ymax = 1000
    ) +
    theme(legend.position = "none")
  p3_with_inset <- p3 + 
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
    annotation_custom(
      ggplotGrob(p3_inset), 
      xmin = lubridate::ymd("2020-03-01"), xmax = lubridate::ymd("2020-12-01"), ymin = -125, ymax = 1000
    ) +
    theme(legend.position = "none")
  
  
  
  
  p1_with_inset/p2_with_inset/p3_with_inset
  
  
  ggsave("figs/JOURNAL_year_plot.png")
  if(tiff){
    ggsave("figs/JOURNAL_year_plot.tiff")
  }
  
  return("figs/JOURNAL_year_plot.png")
}
generate_journal_year_plot_2020 <- function(df, tiff = FALSE, end_year){
  require(tidyverse, quietly = TRUE)
  require(patchwork, quietly = TRUE)
  require(egg, quietly = TRUE)
  
  
  df <- df %>% 
    mutate(gender = ifelse(gender == "female", "women", "men"),
           gender = factor(gender, c("women", "men")))
  ## All author counts
  
  all_authors <- df %>% 
    filter(year %in% 2019:end_year) %>% 
    drop_na(gender) %>% 
    count(gender, year) %>% 
    mutate(year = as.factor(year)) %>% 
    group_by(year) %>% 
    mutate(p = round(100*n/sum(n), 1),
           p = str_c(p, "%"),
           n = 15000)
  
  
  ## All author column plot (Has title plot)
  p1_inset <- df %>% 
    filter(year %in% 2019:end_year) %>% 
    drop_na(gender) %>% 
    count(gender, year) %>% 
    mutate(year = as.factor(year)) %>% 
    ggplot(aes(x = year, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") + 
    # geom_text(data = all_authors, aes(label = p),
    #           colour = "white", position = position_dodge(width = 0.9), size = 3)+
    geom_segment(aes(y=0, yend=30, x=-Inf, xend=-Inf)) +
    labs(x = NULL,
         y = NULL) +
    theme_classic() +
    # # ggtitle("Number of authorships ('000s)") +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          axis.line.y = element_blank()) +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  ## All author line chart
  p1 <- df %>% 
    filter(year %in% 2019:end_year) %>% 
    drop_na(gender) %>% 
    select(date, gender) %>% 
    count(date, gender) %>% 
    mutate(order = "All authors") %>% 
    group_by(date) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    # geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~order, nrow = 2) +
    labs(x = NULL,
         y = NULL,
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    # ggtitle("Total output of all authors") +
    ylim(0, NA) +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14))
  
  
  ## First author counts
  first_author <- df %>% 
    filter(year %in% 2019:end_year, order == "First") %>% 
    drop_na(gender) %>% 
    count(gender, year) %>% 
    mutate(year = as.factor(year)) %>% 
    group_by(year) %>% 
    mutate(p = round(100*n/sum(n), 1),
           p = str_c(p, "%"),
           n = 7500)
  
  
  ## First author column plot (Has y label)
  p2_inset <- df %>% 
    filter(year %in% 2019:end_year, order == "First") %>% 
    drop_na(gender) %>% 
    count(gender, year, order) %>% 
    mutate(year = as.factor(year),
           order = str_c(order, " author")) %>% 
    ggplot(aes(x = year, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") + 
    # geom_text(data = first_author, aes(label = p),
    #           colour = "white", position = position_dodge(width = 0.9), size = 3)+
    geom_segment(aes(y=0, yend=15, x=-Inf, xend=-Inf)) +
    labs(x = NULL,
         y = NULL) +
    theme_classic() +
    # # ggtitle("Number of authorships ('000s)") +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          axis.line.y = element_blank()) +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  ## First author line chart
  
  p2 <- df %>% 
    filter(year %in% 2019:end_year,  order == "First") %>% 
    mutate(order = str_c(order," authors")) %>% 
    drop_na(gender) %>% 
    select(date, gender, order) %>% 
    count(date, gender, order) %>% 
    group_by(date) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    # geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +
    facet_wrap(~order, nrow = 2) +
    labs(x = NULL,
         y = "Number of publications",
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    ylim(0, NA) +
    # ggtitle("Total output of first authors") +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14)) 
  
  
  # Last author counts
  last_author <- df %>% 
    filter(year %in% 2019:end_year, order == "Last") %>% 
    drop_na(gender) %>% 
    count(gender, year) %>% 
    mutate(year = as.factor(year)) %>% 
    group_by(year) %>% 
    mutate(p = round(100*n/sum(n), 1),
           p = str_c(p, "%"),
           n = 7500)
  
  
  ## Last author column plot (Has legend)
  p3_inset <- df %>% 
    filter(year %in% 2019:end_year, order == "Last") %>% 
    drop_na(gender) %>% 
    count(gender, year, order) %>% 
    mutate(year = as.factor(year),
           order = str_c(order, " author")) %>% 
    ggplot(aes(x = year, y = n/1000, fill = gender)) +
    geom_col(position = "dodge", colour = "black") + 
    # geom_text(data = last_author, aes(label = p),
    #           colour = "white", position = position_dodge(width = 0.9), size = 3)+
    geom_segment(aes(y=0, yend=20, x=-Inf, xend=-Inf)) +
    labs(x = NULL,
         y = NULL,
         fill = NULL) +
    theme_classic() +
    # # ggtitle("Number of authorships ('000s)") +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women")) +
    theme(strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          axis.line.y = element_blank(),
          legend.position = "none") +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA)) 
  
  ## Last author line chart
  
  p3 <- df %>% 
    filter(year %in% 2019:end_year,  order == "Last") %>% 
    mutate(order = str_c(order," authors")) %>% 
    drop_na(gender) %>% 
    select(date, gender, order) %>% 
    count(date, gender, order) %>% 
    group_by(date) %>% 
    ggplot(aes(x = date, y = n, colour = gender, shape = gender)) +
    geom_point()+
    geom_line() +
    geom_vline(xintercept = lubridate::as_date("2020-01-01"), lty = 2, alpha = 0.5) +
    # geom_vline(xintercept = lubridate::as_date("2021-01-01"), lty = 2, alpha = 0.5) +s
    facet_wrap(~order, nrow = 2) +
    labs(x = NULL,
         y = NULL,
         colour = "Gender",
         shape = "Gender") +
    theme_classic() +
    # ggtitle("Total output of Last authors") +
    ylim(0, NA) +
    harrypotter::scale_colour_hp_d("Hufflepuff", limits = c("men", "women")) +
    scale_shape(limits = c("men", "women")) +
    theme(strip.background = element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14))
  
  p1_with_inset <- p1 + 
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
      ggplotGrob(p1_inset), 
      xmin = lubridate::ymd("2020-03-01"), xmax = lubridate::ymd("2020-9-01"), ymin = -200, ymax = 2000
    ) +
    theme(legend.position = c(0.25, 0.25),
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "vertical") +
    guides(color = guide_legend(reverse = TRUE, title.position = "left"),
           shape = guide_legend(reverse = TRUE, title.position = "left")) 
  # theme(legend.position = c(0.25, 0.75),
  #       legend.background = element_rect(fill = "transparent"),
  #       legend.direction = "vertical") +
  # guides(color = guide_legend(reverse = TRUE, title.position = "left"),
  #        shape = guide_legend(reverse = TRUE, title.position = "left")) 
  p2_with_inset <- p2 + 
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
    annotation_custom(
      ggplotGrob(p2_inset), 
      xmin = lubridate::ymd("2020-03-01"), xmax = lubridate::ymd("2020-9-01"), ymin = -125, ymax = 1000
    ) +
    theme(legend.position = "none")
  p3_with_inset <- p3 + 
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
    annotation_custom(
      ggplotGrob(p3_inset), 
      xmin = lubridate::ymd("2020-03-01"), xmax = lubridate::ymd("2020-9-01"), ymin = -125, ymax = 1000
    ) +
    theme(legend.position = "none")
  
  
  
  
  p1_with_inset/p2_with_inset/p3_with_inset
  
  
  ggsave("figs/JOURNAL_year_plot_2019_v_2020.png")
  if(tiff){
    ggsave("figs/JOURNAL_year_plot_2019_v_2020.tiff")
  }
  
  return("figs/JOURNAL_year_plot_2019_v_2020.png")
}