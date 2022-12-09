# Author: Matt Ryan
# Date: 07/12/2020

# Source all functions for downloading and cleaning data
# At a minimum, you need pacman installed for this file to work.

## Packages:
pacman::p_load(tidyverse, glue)

## Data download:
download_functions <- list.files("code/download_data_functions/")

download_source_functions <- map(download_functions,
    ~source(glue("code/download_data_functions/{.x}")))

## clean download:
clean_functions <- list.files("code/clean_data_functions/")

clean_source_functions <- map(clean_functions,
    ~source(glue("code/clean_data_functions/{.x}")))

rm("download_functions", "download_source_functions", "clean_functions", "clean_source_functions")
