library(readr)
library(dplyr)
library(magrittr)
library(tidyr)

path <- "data-raw/zip_codes/"

zip_crosswalk <- read_csv(path %p% "ZIP_COUNTY_122017.csv", col_types = "ccdddd")

zip_crosswalk %<>%
  transmute(
    FIPS = county,
    zip,
    pct_pop_in_county = res_ratio * 100,
    business_in_county = bus_ratio * 100,
    total_in_county = tot_ratio * 100)

MSA_zip <- zip_crosswalk %>%
  pull_peers(geog = "MSA", add_info = F) %>%
  left_join(MSA_FIPS, by = "FIPS") %>%
  group_by(zip, MSA) %>%
  summarise(
    pct_in_MSA = sum(pct_pop_in_county),
    business_in_county = sum(business_in_county),
    total_in_county = sum(total_in_county),
    .groups = "drop") %>%
  select(MSA, zip, pct_in_MSA, business_in_county, total_in_county)

FIPS_zip <- zip_crosswalk %>%
  pull_peers(add_info = F) %>%
  select(FIPS, zip, pct_pop_in_county, business_in_county, total_in_county)

FIPS_zip_full_MSA <- zip_crosswalk %>%
  pull_peers(geog = "MSA", add_info = F) %>%
  select(FIPS, zip, pct_pop_in_county, business_in_county, total_in_county)

usethis::use_data(MSA_zip, FIPS_zip, overwrite = TRUE)
usethis::use_data(FIPS_zip_full_MSA, overwrite = TRUE, internal = TRUE)
