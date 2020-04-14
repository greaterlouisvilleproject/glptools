library(readr)
library(dplyr)
library(stringr)
library(magrittr)
source("R/general.R")

# Downloaded from https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html

tract_crosswalk <- read_csv("data-raw/tract_crosswalk/tract_crosswalk.csv",
                            col_types = cols_only(GEOID00 = "c", GEOID10 = "c", POPPCT00 = "n", POPPCT10 = "n"))

attr(tract_crosswalk, 'spec') <- NULL

tract00_tract_10 <- tract_crosswalk %>%
  filter(
    str_sub(GEOID00, 1, 5) == "21111",
    POPPCT00 > 0) %>%
  transmute(
    tract00 = GEOID00,
    tract10 = GEOID10,
    percent = POPPCT00)

tract10_tract_00 <- tract_crosswalk %>%
  filter(
    str_sub(GEOID00, 1, 5) == "21111",
    POPPCT00 > 0) %>%
  transmute(
    tract10 = GEOID10,
    tract00 = GEOID00,
    percent = POPPCT10)

usethis::use_data(tract00_tract_10, overwrite = TRUE)
usethis::use_data(tract10_tract_00, overwrite = TRUE)
