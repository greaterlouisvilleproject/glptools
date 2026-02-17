library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
source("R/operators.R")
source("R/general_utils.R")
source("R/peer_utils.R")
source("R/glp_utils.R")

path <- "data-raw/inflation_and_COLA/"

# Cost of Living
#   Data from BEA interactive mapping tool at https://apps.bea.gov/itable/iTable.cfm?ReqID=70

rpp <- read_csv(path %p% "rppmsa.csv",
                skip = 3,
                col_names = TRUE,
                na = c("", "(NA)"),
                col_types = "c_nnnnnnnnnnnnnnnn",
                n_max = 386)

rpp %<>%
  pivot_longer(-GeoFIPS,
               names_to = "year", names_transform = list(year = as.numeric),
               values_to = "rpp") %>%
  rename(MSA = GeoFIPS) %>%
  left_join(MSA_FIPS, by = "MSA") %>%
  pull_peers(geog = "FIPS") %>%
  select(FIPS, year, rpp) %>%
  complete(FIPS, year = 2000:2026) %>%
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
cpi <- read_csv(path %p% "c-cpi-u.csv",
                skip = 11,
                col_types = "nnnnnnnnnnnnn")

cpi %<>%
  rename(year = Year) %>%
  pivot_longer(Jan:Dec, values_to = "cpi") %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")

COLA_df %<>% left_join(cpi, by = "year")

usethis::use_data(COLA_df, overwrite = TRUE)
