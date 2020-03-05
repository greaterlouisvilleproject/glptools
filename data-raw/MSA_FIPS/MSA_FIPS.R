library(readr)
library(dplyr)
library(magrittr)
source("R/general.R")

MSA_FIPS <- read_csv("data-raw/MSA_FIPS/MSA_definitions.csv", col_types = "cccccc")

MSA_FIPS %<>%
  transmute(
    FIPS = `FIPS State county code`,
    MSA = `CBSA (Blanks are Rural)`) %>%
  right_join(MSA_df, by = "MSA") %>%
  select(MSA, FIPS) %>%
  rbind(c(41180, "MERGED"))

usethis::use_data(MSA_FIPS, overwrite = TRUE)

