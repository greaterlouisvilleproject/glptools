library(readr)
library(dplyr)
library(magrittr)
source("R/operators.R")
source("R/general_utils.R")

# Data from the U.S. Census Burea's Delineation Files Page
# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html

# 2012 MSA definition

# The Dayton MSA code changed in 2018
# the MSA name changed from Dayton to Dayton-Kettering
MSA_df_2012 <- MSA_df %>% mutate(MSA = replace(MSA, MSA == "19430", "19380"))

MSA_FIPS_2012 <- read_csv("data-raw/MSA_FIPS/MSA_definitions.csv", col_types = "cccccc")

MSA_FIPS_2012 %<>%
  transmute(
    FIPS = stringr::str_pad(`FIPS State county code`, 5, "left", "0"),
    MSA = `CBSA (Blanks are Rural)`) %>%
  right_join(MSA_df_2012, by = "MSA") %>%
  select(MSA, FIPS) %>%
  rbind(c("41180", "MERGED"))

# 2020 MSA definition
MSA_FIPS <- read_csv("data-raw/MSA_FIPS/list1_2020.csv", col_types = "c______c_cc_", skip = 2)

MSA_FIPS %<>%
  transmute(
    FIPS = stringr::str_pad(`FIPS State Code` %p% `FIPS County Code`, 5, "left", "0"),
    MSA = `CBSA Code`,
    county = `County/County Equivalent`) %>%
  right_join(MSA_df, by = "MSA") %>%
  select(MSA, FIPS, county)

MSA_FIPS_info <- MSA_FIPS

MSA_FIPS %<>%
  select(MSA, FIPS) %>%
  rbind(c("41180", "MERGED"))

MSA_FIPS_core_county <- MSA_FIPS %>%
  filter(FIPS %in% FIPS_df_one_stl$FIPS)

usethis::use_data(MSA_FIPS, MSA_FIPS_2012, MSA_FIPS_core_county, MSA_FIPS_info, overwrite = TRUE)

