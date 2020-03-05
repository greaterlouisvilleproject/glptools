library(readr)
library(dplyr)
library(magrittr)
source("R/general.R")

path <- "data-raw/basic_info/"

FIPS_info <- read_csv(path %p% "FIPS_data.csv", col_types = "ccddccc")

attr(FIPS_info, 'spec') <- NULL

usethis::use_data(FIPS_info, overwrite = TRUE)


FIPS_df <- FIPS_info %>% select(-state_abbr, -state, -county)

usethis::use_data(FIPS_df, overwrite = TRUE)


FIPS_df_one_stl <- filter(FIPS_df, city != "St. Louis" | FIPS == "MERGED")

usethis::use_data(FIPS_df_one_stl, overwrite = TRUE)


FIPS_df_two_stl <- filter(FIPS_df, FIPS != "MERGED")

usethis::use_data(FIPS_df_two_stl, overwrite = TRUE)


MSA_df <- read_csv(path %p% "MSA_data.csv", col_types = "ccdd")

attr(MSA_df, 'spec') <- NULL

usethis::use_data(MSA_df, overwrite = TRUE)


state_df <- read_csv(path %p% "state_data.csv", col_types = "dccdd")

attr(state_df, 'spec') <- NULL

usethis::use_data(state_df, overwrite = TRUE)
