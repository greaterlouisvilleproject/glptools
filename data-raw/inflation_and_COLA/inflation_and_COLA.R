library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
source("R/general.R")

path <- "data-raw/inflation_and_COLA/"

# Cost of Living
#   Data from BEA interactive mapping tool at https://apps.bea.gov/itable/iTable.cfm?ReqID=70

rpp <- read_csv(path %p%"rppmsa.csv", skip = 4, col_names = TRUE, na = c("", "(NA)"),
                col_types = "c_n_nnnnnnnnnnn", n_max = 1544)

rpp %<>%
  filter(LineCode == 1) %>%
  select(-LineCode) %>%
  pivot_longer(-GeoFips,
               names_to = "year", names_transform = list(year = as.numeric),
               values_to = "rpp") %>%
  rename(MSA = GeoFips) %>%
  left_join(MSA_FIPS, by = "MSA") %>%
  pull_peers(geog = "FIPS") %>%
  select(FIPS, year, rpp) %>%
  complete(FIPS, year = 2000:2020) %>%
  group_by(FIPS) %>%
  fill(rpp, .direction = "updown")

COLA_df <- rpp %>%
  group_by(year) %>%
  mutate(rpp_lou = rpp[FIPS == 21111],
         rpp_index = rpp_lou / rpp) %>%
  ungroup() %>%
  select(-rpp, -rpp_lou)

# Inflation
#   Data from C-CPI-U "Top Picks" https://www.bls.gov/cpi/data.htm

# read_csv removes data notes from numbers and reports problems
cpi <- read_csv(path %p% "c-cpi-u.csv", skip = 11, col_types = "nnnnnnnnnnnnn")

cpi %<>%
  rename(year = Year) %>%
  pivot_longer(Jan:Dec, values_to = "cpi") %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")

COLA_df %<>% left_join(cpi, by = "year")

usethis::use_data(COLA_df, overwrite = TRUE)
