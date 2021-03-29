library(readr)
library(dplyr)
library(magrittr)
library(tidyr)
source("R/operators.R")
source("R/general_utils.R")
source("R/peer_utils.R")
source("R/glp_utils.R")

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

# tract_zip <- read_csv(path %p% "ZIP_TRACT_122017.csv", col_types = "ccdddd")
#
# zip_to_tract <- tract_zip %<>%
#   transmute(
#     FIPS = str_sub(tract, 1, 5),
#     tract,
#     zip,
#     pct_zip_pop_in_tract = res_ratio * 100,
#     pct_zip_business_in_tract = bus_ratio * 100,
#     pct_zip_total_in_tract = tot_ratio * 100) %>%
#   filter(str_sub(FIPS, 1, 2) == "21")
#
# tract_to_zip <- tract_zip %>%
#   group_by(tract) %>%
#   mutate(
#     pct_tract_pop_in_zip = pct_zip_pop_in_tract / sum(pct_zip_pop_in_tract) * 100) %>%
#   filter(pct_tract_pop_in_zip != 0)
#
# table(tract_to_zip$pct_tract_pop_in_zip == 100)
# mean(tract_to_zip$pct_tract_pop_in_zip == 100)
#
# hist(tract_to_zip$pct_tract_pop_in_zip, freq = F, breaks = 100)

usethis::use_data(MSA_zip, FIPS_zip, FIPS_zip_full_MSA, overwrite = TRUE)
