pacman::p_load(tidyverse, gt, targets)

tar_load(preprocess_data)
tar_load(model_data)

prop_z_test <- function(y, ...){
  succ <- y[, 1]
  
  p <- sum(succ)/sum(y)
  p1 <- succ[1]/sum(y[1,])
  p2 <- succ[2]/sum(y[2,])
  
  z <- (p1 - p2)/(sqrt(p*(1-p)*(1/sum(y[1,]) + 1/sum(y[2,]))))
  p_val <- 2*pnorm(abs(z), lower.tail = FALSE)
  
  if(p_val < 0.05 & sign(z) == 1){
    res <- "p1 is larger than p2"
  }else if(p_val < 0.05 & sign(z) == -1){
    res <- "p2 is larger than p1"
  }else{
    res <- "there is no statistically significant difference"
  }
  print(
    glue::glue(
      "The test statistic is z = {z}.
      The p-value is p = {p_val}.
      This suggests {res}."
    )
  )
  return(
    invisible(
      tibble(z = z,
             p.value = p_val,
             p1 = p1,
             p2 = p2)
    )
  )
}



#  Look at 2019 and 2020 to compare data ----------------------------------


preprocess_data %>% 
  filter(year >= 2019, year < 2022) %>% 
  drop_na(gender) %>% 
  skimr::skim_without_charts()

# Testing gender vs year 2019 vs 2020 --------------------------------------------------

# All authors

## Counts
gender_vs_year_counts <- model_data %>% 
  filter(date < lubridate::as_date("2022-01-01"), # Compare 2019 and 2020
         date >= lubridate::as_date("2019-01-01")) %>% 
  mutate(year = lubridate::year(date)) %>% # Pull out year
  group_by(year, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% # Find total number
  drop_na() %>% 
  pivot_wider(names_from = gender, values_from = n) 

gender_vs_year_matrix <- gender_vs_year_counts %>% 
  select(-year) %>% 
  as.matrix()

gender_vs_year_counts

## Proportion test
(year_test <- chisq.test(gender_vs_year_matrix))

# First authors

## Counts
gender_vs_year_counts_first <- model_data %>% 
  filter(order == "First") %>% 
  filter(date < lubridate::as_date("2022-01-01"), 
         date >= lubridate::as_date("2019-01-01")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  drop_na() %>% 
  pivot_wider(names_from = gender, values_from = n) 

gender_vs_year_matrix_first <- gender_vs_year_counts_first %>% 
  select(-year) %>% 
  as.matrix()

gender_vs_year_counts_first

## Proportion test
(year_test_first <- chisq.test(gender_vs_year_matrix_first))

# Last authors

## Counts
gender_vs_year_counts_last <- model_data %>% 
  filter(order == "Last") %>% 
  filter(date < lubridate::as_date("2022-01-01"), 
         date >= lubridate::as_date("2019-01-01")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  drop_na() %>% 
  pivot_wider(names_from = gender, values_from = n) 

gender_vs_year_matrix_last <- gender_vs_year_counts_last %>% 
  select(-year) %>% 
  as.matrix()

gender_vs_year_counts_last

## Proportion test
(year_test_last <- chisq.test(gender_vs_year_matrix_last))


# Gender vs state - 2020 Mar to Dec ---------------------------------------------------------

# All authors
## Counts
gender_vs_state_counts <- model_data %>% 
  filter(date < lubridate::as_date("2021-01-01"), # Look at march to dec
         date >= lubridate::as_date("2020-03-01")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(publishing_states, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  drop_na() %>% # Remove NA publishing states and authors
  pivot_wider(names_from = gender, values_from = n) %>% 
  slice(-5) # Remove NA states

gender_vs_state_matrix <- gender_vs_state_counts %>% 
  select(-publishing_states) %>% 
  as.matrix()

gender_vs_state_counts

## Chisquared test
(state_test <- chisq.test(gender_vs_state_matrix))

# First authors
## Counts
gender_vs_state_counts_first <- model_data %>% 
  filter(order == "First") %>% 
  filter(date < lubridate::as_date("2021-01-01"), 
         date >= lubridate::as_date("2020-03-01")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(publishing_states, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  drop_na() %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  slice(-5)

gender_vs_state_matrix_first <- gender_vs_state_counts_first %>% 
  select(-publishing_states) %>% 
  as.matrix()

gender_vs_state_counts_first

## Chisquared test
(state_test_first <- chisq.test(gender_vs_state_matrix_first))

# Last authors
## Counts
gender_vs_state_counts_last <- model_data %>% 
  filter(order == "Last") %>% 
  filter(date < lubridate::as_date("2021-01-01"), 
         date >= lubridate::as_date("2020-03-01")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(publishing_states, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  drop_na() %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  slice(-5)

gender_vs_state_matrix_last <- gender_vs_state_counts_last %>% 
  select(-publishing_states) %>% 
  as.matrix()

gender_vs_state_counts_last

## Chisquared test
(state_test_last <- chisq.test(gender_vs_state_matrix_last))


# Covid vs Gender 2020 ----------------------------------------------------

# All authors
## Counts
gender_vs_covid_counts <- model_data %>% 
  filter(date < lubridate::as_date("2021-01-01"), 
         date > lubridate::as_date("2019-12-01")) %>% 
  group_by(covid, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  drop_na() %>% # Remove unmatched gender
  pivot_wider(names_from = gender, values_from = n)

gender_vs_covid_matrix <- gender_vs_covid_counts %>% 
  select(-covid) %>% 
  as.matrix()

gender_vs_covid_counts

## Proportion test
(covid_test <- prop_z_test(gender_vs_covid_matrix, correct = FALSE))

# First authors
## Counts
gender_vs_covid_counts_first <- model_data %>% 
  filter(order == "First") %>% 
  filter(date < lubridate::as_date("2021-01-01"), 
         date > lubridate::as_date("2019-12-01")) %>% 
  group_by(covid, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  drop_na() %>% 
  pivot_wider(names_from = gender, values_from = n)

gender_vs_covid_matrix_first <- gender_vs_covid_counts_first %>% 
  select(-covid) %>% 
  as.matrix()

gender_vs_covid_counts_first

## Proportion test
(covid_test_first <- prop_z_test(gender_vs_covid_matrix_first, correct = FALSE))

# Last authors
## Counts
gender_vs_covid_counts_last <- model_data %>% 
  filter(order == "Last") %>% 
  filter(date < lubridate::as_date("2021-01-01"), 
         date > lubridate::as_date("2019-12-01")) %>% 
  group_by(covid, gender) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  drop_na() %>% 
  pivot_wider(names_from = gender, values_from = n)

gender_vs_covid_matrix_last <- gender_vs_covid_counts_last %>% 
  select(-covid) %>% 
  as.matrix()

gender_vs_covid_counts_last

## Proportion test
(covid_test_last <- prop_z_test(gender_vs_covid_matrix_last, correct = FALSE))


# extra data --------------------------------------------------------------
# 
tar_load(preprocess_data_2021)

covid_2021_mat <- preprocess_data_2021 %>% 
  filter(year %in% 2021:2021) %>%
  drop_na(gender) %>% 
  count(covid, gender) %>%
  pivot_wider(names_from = gender, values_from = n) %>% 
  select(-covid) %>% 
  as.matrix()
(covid_2021_prop_test <- prop_z_test(covid_2021_mat))

rm(preprocess_data_2021)

# Creating table output ---------------------------------------------------

## Adjust the P-values
p_vals <- c(
  covid_test$p.value,
  year_test$p.value,
  state_test$p.value,
  covid_test_first$p.value,
  year_test_first$p.value,
  state_test_first$p.value,
  covid_test_last$p.value,
  year_test_last$p.value,
  state_test_last$p.value,
  0.228669314171518 # HIV p-value
)
p_vals_adj <- p.adjust(p_vals, method = "fdr")

## Make COVID tables
t1 <- gender_vs_covid_counts %>% 
  mutate(authors = c("All authors", ""),
         covid = ifelse(covid, "COVID-related", "non-COVID-related")) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      covid = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[1],
      `Adjusted P-value` = p_vals_adj[1]
    )
  )
t4 <- gender_vs_covid_counts_first %>% 
  mutate(authors = c("First authors", ""),
         covid = ifelse(covid, "COVID-related", "non-COVID-related")) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      covid = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[4],
      `Adjusted P-value` = p_vals_adj[4]
    )
  )
t7 <- gender_vs_covid_counts_last %>% 
  mutate(authors = c("Last authors", ""),
         covid = ifelse(covid, "COVID-related", "non-COVID-related")) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      covid = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[7],
      `Adjusted P-value` = p_vals_adj[7]
    )
  )

## Make year tables
t2 <- gender_vs_year_counts %>% 
  mutate(authors = c("All authors", "", ""),
         year = as.character(year)) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      year = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[2],
      `Adjusted P-value` = p_vals_adj[2]
    )
  )
t5 <- gender_vs_year_counts_first %>% 
  mutate(authors = c("First authors", "", ""),
         year = as.character(year)) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      year = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[5],
      `Adjusted P-value` = p_vals_adj[5]
    )
  )
t8 <- gender_vs_year_counts_last %>% 
  mutate(authors = c("Last authors", "", ""),
         year = as.character(year)) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      year = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[8],
      `Adjusted P-value` = p_vals_adj[8]
    )
  )

## Make state tables
t3 <- gender_vs_state_counts %>% 
  mutate(authors = c("All authors", "", "", "")) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      publishing_states = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[3],
      `Adjusted P-value` = p_vals_adj[3]
    )
  )
t6 <- gender_vs_state_counts_first %>% 
  mutate(authors = c("First authors", "", "", "")) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      publishing_states = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[6],
      `Adjusted P-value` = p_vals_adj[6]
    )
  )
t9 <- gender_vs_state_counts_last %>% 
  mutate(authors = c("Last authors", "", "", "")) %>% 
  select(authors, everything()) %>% 
  bind_rows(
    tibble(
      authors = "",
      publishing_states = "",
      female = NA,
      male = NA,
      `P-value` = p_vals[9],
      `Adjusted P-value` = p_vals_adj[9]
    )
  )

## Make big table
t2 %>% 
  bind_rows(t5, t8) %>% 
  bind_rows(t3, t6, t9) %>%
  bind_rows(t1, t4, t7) %>%
  mutate(across(c(-female, -male), as.character)) %>% 
  pivot_longer(-c(female, male, `P-value`, `Adjusted P-value`, authors)) %>% 
  drop_na(value) %>% 
  select(name, authors, value, female, male, `P-value`, `Adjusted P-value`) %>% 
  mutate(across(c(`P-value`, `Adjusted P-value`), as.numeric)) %>% 
  mutate(name = case_when(name == "covid" ~ "Research topic",
                          name == "year" ~ "Year",
                          TRUE ~ "Author state"),
         authors = ifelse(authors == "", NA, authors)) %>%
  fill(authors, .direction = "down") %>% 
  group_by(name, authors) %>% 
  gt(rowname_col = "value") %>% 
  fmt_number(columns = c("P-value", "Adjusted P-value"),
             decimals = 2,n_sigfig = 2) %>% 
  cols_label(female = "Female",
             male = "Male",
             "P-value" = "Raw",
             "Adjusted P-value" = "Adjusted") %>% 
  tab_style(
    style = cell_fill(),
    locations = cells_body(
      columns = `Adjusted P-value`,
      rows = `Adjusted P-value` < 0.05
    )
  ) %>% 
  tab_header(
    title = "Australian author publications."
  ) %>% 
  tab_spanner(
    label = "Author sex",
    columns = c(female, male)
  ) %>% 
  tab_spanner(
    label = "P-values",
    columns = c("P-value", "Adjusted P-value")
  )
