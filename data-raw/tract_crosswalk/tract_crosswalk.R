library(readr)
library(dplyr)
library(stringr)
library(magrittr)
source("R/readers.R")

# 2000 to 2010

# Downloaded from https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html
tract_crosswalk1 <- any_time("data-raw/tract_crosswalk/files",
                            starting_year = NA,
                            read_fxn = function(x) read_csv(x,
                                                            col_names = c("GEOID00", "POP00", "HU00",
                                                                          "GEOID10", "POP10", "HU10",
                                                                          "POPPCT00", "POPPCT10",
                                                                          "HUPCT00", "HUPCT10"),
                                                            col_types = "___cnn______cnn__________nn_nn"))

attr(tract_crosswalk1, 'spec') <- NULL

# Filter to tracts where the original and destination tract are in a peer county.
# Most are boundary changes that do not impact population figures. (Including Louisville)
# Some include population shifts in or out of peer counties, usually of between 0%-1%,
# with a maximum of 2.8%. These are ignored for simplicity.

tract_crosswalk1 %<>%
  filter(
    str_sub(GEOID00, 1, 5) %in% FIPS_df_two_stl$FIPS &
    str_sub(GEOID10, 1, 5) %in% FIPS_df_two_stl$FIPS)

tract00_tract10 <- tract_crosswalk1 %>%
  transmute(
    tract00 = GEOID00,
    tract10 = GEOID10,
    percent = POPPCT00,
    housing_percent = HUPCT00)

tract10_tract00 <- tract_crosswalk1 %>%
  filter(
    str_sub(GEOID00, 1, 5) %in% FIPS_df_two_stl$FIPS) %>%
  transmute(
    tract10 = GEOID10,
    tract00 = GEOID00,
    percent = POPPCT10,
    housing_percent = HUPCT10)

# 2010 to 2020

# Downloaded from https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html

tract_crosswalk2 <- read_delim("data-raw/tract_crosswalk/tab20_tract20_tract10_natl.txt",
                               delim = "|",
                               skip = 1,
                               col_names = c("GEOID20", "AREA20", "GEOID10", "AREA10", "AREA_overlap"),
                               col_types = c("_c_n____c_n___n_"))


attr(tract_crosswalk2, 'spec') <- NULL

tract_crosswalk2 %<>%
  filter(
    str_sub(GEOID10, 1, 5) %in% FIPS_df_two_stl$FIPS &
    str_sub(GEOID20, 1, 5) %in% FIPS_df_two_stl$FIPS)

tract10_tract20 <- tract_crosswalk2 %>%
  transmute(
    tract10 = GEOID10,
    tract20 = GEOID20,
    percent = AREA_overlap / AREA10 * 100)

tract20_tract10 <- tract_crosswalk2 %>%
  filter(
    str_sub(GEOID10, 1, 5) %in% FIPS_df_two_stl$FIPS) %>%
  transmute(
    tract20 = GEOID20,
    tract10 = GEOID10,
    percent = AREA_overlap / AREA20 * 100)

usethis::use_data(tract00_tract10, tract10_tract00,
                  tract10_tract20, tract20_tract10,
                  overwrite = TRUE)
