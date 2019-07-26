library(readr)
source("R/general.R")

path <- "data-raw/basic_info/"

FIPS_df <- read_csv(path %p% "FIPS_data.csv", col_types = "ccdd")

attr(FIPS_df, 'spec') <- NULL

usethis::use_data(FIPS_df, overwrite = TRUE)

MSA_df <- read_csv(path %p% "MSA_data.csv", col_types = "ccdd")

attr(MSA_df, 'spec') <- NULL

usethis::use_data(MSA_df, overwrite = TRUE)
