library(readr)
library(dplyr)
library(magrittr)

path <- "data-raw/population/"


population_df <- read_csv(path %p% "population_data.csv", col_types = "cnn")

attr(population_df, 'spec') <- NULL

usethis::use_data(population_df, overwrite = TRUE)


population_df_merged <- read_csv(path %p% "population_data_merged.csv", col_types = "cnn")

attr(population_df_merged, 'spec') <- NULL

usethis::use_data(population_df_merged, overwrite = TRUE)
