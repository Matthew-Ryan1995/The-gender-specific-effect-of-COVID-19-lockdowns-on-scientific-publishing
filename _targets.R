library(targets)
library(tarchetypes)
library(tidyverse)

code_files <- glue::glue("code/{list.files('code/')}")
code_files <- code_files[-c(1, 2)] # Removing for now
walk(code_files, source)
# rm(code_files, sourced_files)

# Set target-specific options such as packages.
tar_option_set(packages = "dplyr")

# End this file with a list of target objects.
# To get original plots for the 2020 paper, change end_year to 2021
list(
  tar_target(
    return_plot,
    TRUE
  ),
  tar_target(
    full_data_file,
    # "data/September_data.csv",
    "data/2022_July_download.csv",
    format = "file"
  ),
  tar_target(
    overall_end_year,
    2022
  ),
  tar_target(
    covid_plot_end_year,
    2022
  ),
  tar_target(
    full_data,
    read_csv(full_data_file, col_types = "cccdddccccccddddddccdcdddccccdcc")
  ),
  tar_target(full_data_state,
             get_institute_and_state(full_data)),
  tar_target(final_data,
             extra_gender_matching(full_data_state)),
  tar_render(
    data_dictionary_report,
    "analysis/02_data_dictionary.Rmd"
  ),
  tar_render(
    univariate_report,
    "analysis/03_univariate_exploration.Rmd"
  ),
  tar_render(
    bivariate_report, error = "continue",
    "analysis/04_bivariate_analysis.Rmd"
  ),
  tar_target(
    preprocess_data,
    create_preprocess_data(final_data, end_year = overall_end_year)
  ),
  tar_target(
    overall_plot,
    create_overall_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_order_plot,
    create_overall_order_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_paper_plot,
    create_overall_paper_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_covid_plot,
    create_overall_covid_plot(preprocess_data, return_plot = return_plot, covid.only = FALSE, end_year = overall_end_year)
  ),
  tar_target(
    overall_state_plot,
    create_overall_state_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_order_covid_plot,
    create_overall_order_covid_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_order_paper_plot,
    create_overall_order_paper_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_order_state_plot,
    create_overall_order_state_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_covid_paper_plot,
    create_overall_covid_paper_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_covid_state_plot,
    create_overall_covid_state_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_target(
    overall_paper_state_plot,
    create_overall_paper_state_plot(preprocess_data, return_plot = return_plot, end_year = overall_end_year)
  ),
  tar_render(
    plots_report,
    "analysis/05_plots.Rmd",
    params = list(plotly = return_plot)
  ),
  tar_render(
    analysis_report,
    "analysis/06_analysis_for_paper.Rmd"
  ),
  tar_target(
    model_data, 
    create_model_data(preprocess_data)
  ),
  tar_target(
    nature_covid_plot,
    generate_covid_inset_plot(preprocess_data, end_year = covid_plot_end_year),
    format = "file"
  ),
  tar_target(
    tiff,
    TRUE
  ),
  tar_target(
    journal_covid_plot,
    generate_journal_covid_inset_plot(preprocess_data, tiff, end_year = covid_plot_end_year),
    format = "file"
  ),
  tar_target(
    journal_covid_plot_2020,
    generate_journal_covid_inset_plot_2020(preprocess_data, tiff, end_year = 2021),
    format = "file"
  ),
  tar_target(
    journal_covid_plot_first_author,
    generate_journal_covid_inset_plot_first_author(preprocess_data, tiff, end_year = covid_plot_end_year),
    format = "file"
  ),
  tar_target(
    journal_year_plot,
    generate_journal_year_plot(preprocess_data, tiff, end_year = covid_plot_end_year - 1),
    format = "file"
  ),
  tar_target(
    journal_year_plot_2020,
    generate_journal_year_plot_2020(preprocess_data, tiff, end_year = 2020),
    format = "file"
  ),
  tar_target(
    journal_state_plot,
    generate_journal_state_plot(model_data, tiff),
    format = "file"
  ),
  tar_target(
    journal_state_covid_plot,
    generate_journal_state_covid_plot(model_data, tiff),
    format = "file"
  ),
  tar_target(
    data_file_2021,
    "data/2022_July_download.csv",
    format = "file"
  ),
  tar_target(
    full_data_2021,
    read_csv(data_file_2021, col_types = "cccdddccccccddddddccdcdddccccdcc")
  ),
  tar_target(full_data_state_2021,
             get_institute_and_state(full_data_2021)),
  tar_target(final_data_2021,
             extra_gender_matching(full_data_state_2021)),
  tar_target(
    preprocess_data_2021,
    create_preprocess_data(final_data_2021, end_year = 2022)
  ),
  tar_target(
    covid_bar_chart,
    create_covid_barchart_2020_vs_2021(preprocess_data_2021, tiff)
  )
  # tar_render(
  #   plots_for_paper,
  #   "analysis/09_plots_for_paper.Rmd"
  # )
)
