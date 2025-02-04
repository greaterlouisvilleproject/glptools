library(readr)
library(dplyr)
library(magrittr)
source("R/operators.R")
source("R/general_utils.R")

path <- "data-raw/basic_info/"

# FIPS_info
FIPS_info <- read_csv(path %p% "FIPS_data.csv", col_types = "ccddccc")

attr(FIPS_info, 'spec') <- NULL

FIPS_info %<>% mutate(FIPS = stringr::str_pad(FIPS, 5, "left", "0"))

# FIPS_df
FIPS_df <- FIPS_info %>% select(-state_abbr, -state, -county)

# FIPS_df_one_stl
FIPS_df_one_stl <- filter(FIPS_df, city != "St. Louis" | FIPS == "MERGED")

FIPS_df_one_stl %<>% mutate(FIPS = stringr::str_pad(FIPS, 5, "left", "0"))

# FIPS_df_two_stl
FIPS_df_two_stl <- filter(FIPS_df, FIPS != "MERGED")

FIPS_df_two_stl %<>% mutate(FIPS = stringr::str_pad(FIPS, 5, "left", "0"))

# MSA_df
MSA_df <- read_csv(path %p% "MSA_data.csv", col_types = "ccdd")

attr(MSA_df, 'spec') <- NULL

# state_df
state_df <- read_csv(path %p% "state_data.csv", col_types = "dccdd")

state_df %<>% mutate(FIPS = stringr::str_pad(FIPS, 2, "left", "0"))

attr(state_df, 'spec') <- NULL

# state FIPS

all_states   <- read_delim(path %p% "state_fips.txt", delim = "|", col_types = "cc_c")
all_counties <- read_delim(path %p% "county_fips.txt", delim = "|", col_types = "_cc_c__")

all_states %<>%
  transmute(
    state_FIPS = STATEFP,
    state = STATE_NAME,
    state_abbr = STATE)

all_counties %<>%
  transmute(
    FIPS = STATEFP %p% COUNTYFP,
    state_FIPS = STATEFP,
    county = COUNTYNAME)

all_counties %<>%
  left_join(all_states, by = "state_FIPS")

all_counties %<>%
  transmute(
    FIPS,
    county,
    state,
    state_abbr)

usethis::use_data(FIPS_info, FIPS_df, FIPS_df_one_stl, FIPS_df_two_stl, MSA_df, state_df, all_states, all_counties, overwrite = TRUE)
