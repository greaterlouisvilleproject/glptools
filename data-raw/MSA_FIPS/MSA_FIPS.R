library(readr)
library(dplyr)
library(magrittr)
source("R/general.R")

# 2012 MSA definition

# The Dayton MSA code changed in 2018 when
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

# 2018 MSA definition
MSA_FIPS <- read_csv("data-raw/MSA_FIPS/list1_Sep_2018.csv", col_types = "c________cc_", skip = 2)

MSA_FIPS %<>%
  transmute(
    FIPS = stringr::str_pad(`FIPS State Code` %p% `FIPS County Code`, 5, "left", "0"),
    MSA = `CBSA Code`) %>%
  right_join(MSA_df, by = "MSA") %>%
  select(MSA, FIPS) %>%
  rbind(c("41180", "MERGED"))

MSA_FIPS_core_county <- MSA_FIPS %>%
  filter(FIPS %in% FIPS_df_one_stl$FIPS)

usethis::use_data(MSA_FIPS_2012, overwrite = TRUE)
usethis::use_data(MSA_FIPS, overwrite = TRUE)
usethis::use_data(MSA_FIPS_core_county, overwrite = TRUE)

