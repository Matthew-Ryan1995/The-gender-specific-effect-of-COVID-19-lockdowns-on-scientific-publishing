create_covid_barchart_2020_vs_2021 <- function(df, tiff = FALSE){
  
  covid_numbers <- 
    df %>% 
    drop_na(gender) %>% 
    filter(year %in% c(2020, 2021), !(year == 2019 & covid), covid) %>% 
    count(year, gender, covid) %>% 
    group_by(year) %>% 
    mutate(p = round(n/sum(n), 3)*100,
           n = 800,
           p = str_c(p, "%"),
           gender = ifelse(gender == "female", "women", "men"),
           gender = factor(gender, c("women", "men")))
  
  p <- df %>% 
    drop_na(gender) %>% 
    filter(year %in% c(2020, 2021), !(year == 2019 & covid), covid) %>% 
    count(year, gender, covid) %>% 
    mutate(gender = ifelse(gender == "female", "women", "men"),
           gender = factor(gender, c("women", "men"))) %>% 
    ggplot(aes(x = gender, y = n, fill = gender)) +
    geom_col(position = "dodge", show.legend = FALSE, colour = "black") +
    geom_text(data = covid_numbers, aes(label = p), 
              size = 3,
              colour = "white") + # PLAYING WITH SIZE
    labs(x = NULL, y = "Number of Publications") +
    facet_wrap(~year) +
    # ggtitle("Publication of COVID related articles for 2020 and 2021") +
    theme_classic() +
    theme(plot.title = element_text(size = 16),
          strip.background =element_rect(fill="gray70"),
          strip.text = element_text(face = "bold", size = 14),
          axis.line.y = element_blank()) +
    geom_segment(aes(y=0, yend=2001, x=-Inf, xend=-Inf)) +
    harrypotter::scale_fill_hp_d("Hufflepuff", limits = c("men", "women"))
    # scale_fill_brewer(limits = c("men", "women"), palette = "Paired")
  
  ggsave("figs/JOURNAL_covid_barchart.png", plot = p, height = 5, width = 7)
  
  if(tiff){
    ggsave("figs/JOURNAL_covid_barchart.tiff", plot = p, height = 5, width = 7)
  }
  
  return("figs/JOURNAL_covid_barchart.png")
}