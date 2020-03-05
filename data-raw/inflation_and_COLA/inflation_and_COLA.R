library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
source("R/general.R")

path <- "data-raw/inflation_and_COLA/"

# Cost of Living
#   Data from BEA interactive mapping tool at https://apps.bea.gov/itable/iTable.cfm?ReqID=70

rpp <- read_csv(path %p%"rppmsa.csv", skip = 4, col_names = TRUE, na = c("", "(NA)"),
                col_types = "ccncnnnnnnnnnn", n_max = 1540)

rpp$`2000` <- rpp$`2008`
rpp$`2001` <- rpp$`2008`
rpp$`2002` <- rpp$`2008`
rpp$`2003` <- rpp$`2008`
rpp$`2004` <- rpp$`2008`
rpp$`2005` <- rpp$`2008`
rpp$`2006` <- rpp$`2008`
rpp$`2007` <- rpp$`2008`
rpp$`2018` <- rpp$`2017`
rpp$`2019` <- rpp$`2017`

rpp <- rpp %>%
  filter(LineCode == 1) %>%
  select(-GeoName, -LineCode, -Description) %>%
  gather(-GeoFips, key = year, value = "rpp") %>%
  mutate(year = as.numeric(year)) %>%
  rename(MSA = GeoFips) %>%
  add_FIPS_to_MSA() %>%
  pull_peers_FIPS() %>%
  select(FIPS, year, rpp)

COLA_df <- rpp %>%
  group_by(year) %>%
  mutate(rpp_lou = rpp[FIPS == 21111],
         rpp_index = rpp_lou / rpp) %>%
  ungroup() %>%
  select(-rpp, -rpp_lou)

# Inflation
#   Data from C-CPI-U "Top Picks" https://www.bls.gov/cpi/data.htm

cpi <- read_csv(path %p% "c-cpi-u.csv", skip = 11, col_types = "nnnnnnnnnnnnn")

cpi %<>%
  rename(year = Year) %>%
  gather(-year, key = month, value = cpi) %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 2000)

COLA_df %<>% left_join(cpi, by = "year")

usethis::use_data(COLA_df, overwrite = TRUE)
