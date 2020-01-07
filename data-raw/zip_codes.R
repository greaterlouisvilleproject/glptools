library(readr)
library(dplyr)
library(magrittr)
library(tidyr)
source("R/general.R")

path <- "data-raw/zip_codes/"

zip_pop <- read_csv(path %p% "ACS_17_5YR_B01003_with_ann.csv", skip = 1, col_types = "cccd_")
zip_crosswalk <- read_csv(path %p% "ZIP_COUNTY_122017.csv", col_types = "ccdddd")

zip_pop %<>%
  transmute(
    zip = Id2,
    population = `Estimate; Total`)

zip_crosswalk %<>%
  left_join(zip_pop, by = "zip") %>%
  transmute(
    FIPS = county,
    zip,
    population_total = population %>% replace_na(0),
    population_in_FIPS = population_total * res_ratio %>% replace_na(0),
    pct_in_county = res_ratio * 100)

MSA_zip <- zip_crosswalk %>%
  pull_peers_FIPS(county_filter = "MSA_counties") %>%
  left_join(MSA_FIPS, by = "FIPS") %>%
  group_by(zip, MSA) %>%
  summarise(
    population_total = mean(population_total),
    population_in_MSA = sum(population_in_FIPS),
    pct_in_MSA = sum(pct_in_county)) %>%
  ungroup() %>%
  left_join(MSA_df, by = "MSA") %>%
  select(MSA, zip, population_total, population_in_MSA)

FIPS_zip <- zip_crosswalk %>%
  pull_peers_FIPS() %>%
  select(FIPS, zip, population_total, population_in_FIPS)

usethis::use_data(FIPS_zip, overwrite = TRUE)
usethis::use_data(MSA_zip, overwrite = TRUE)
