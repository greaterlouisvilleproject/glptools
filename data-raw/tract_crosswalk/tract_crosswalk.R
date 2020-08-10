library(readr)
library(dplyr)
library(stringr)
library(magrittr)
source("R/readers.R")

# Downloaded from https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html
tract_crosswalk <- any_time("data-raw/tract_crosswalk/files",
                            starting_year = NA,
                            read_fxn = function(x) read_csv(x,
                                                            col_names = c("GEOID00", "POP00", "HU00",
                                                                          "GEOID10", "POP10", "HU10",
                                                                          "POPPCT00", "POPPCT10",
                                                                          "HUPCT00", "HUPCT10"),
                                                            col_types = "___cnn______cnn__________nn_nn"))

attr(tract_crosswalk, 'spec') <- NULL

# Filter to tracts where the original and destination tract are in a peer county.
# Most are boundary changes that do not impact population figures. (Including Louisville)
# Some include population shifts in or out of peer counties, usually of between 0%-1%,
# with a maximum of 2.8%. These are ignored for simplicity.

tract_crosswalk %<>%
  filter(
    str_sub(GEOID00, 1, 5) %in% FIPS_df_two_stl$FIPS &
    str_sub(GEOID10, 1, 5) %in% FIPS_df_two_stl$FIPS)

tract00_tract_10 <- tract_crosswalk %>%

  transmute(
    tract00 = GEOID00,
    tract10 = GEOID10,
    percent = POPPCT00,
    housing_percent = HUPCT00)

tract10_tract_00 <- tract_crosswalk %>%
  filter(
    str_sub(GEOID00, 1, 5) %in% FIPS_df_two_stl$FIPS) %>%
  transmute(
    tract10 = GEOID10,
    tract00 = GEOID00,
    percent = POPPCT10,
    housing_percent = HUPCT10)

usethis::use_data(tract00_tract_10, overwrite = TRUE)
usethis::use_data(tract10_tract_00, overwrite = TRUE)
