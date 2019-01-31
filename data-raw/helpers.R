library(tidyverse)
library(magrittr)
source("R/general.R")

#Population
population_df <- read_csv("data-raw/population_data.csv")

population_df %<>%
  mutate(FIPS = as.numeric(FIPS)) %>%
  select(FIPS, year, population)

usethis::use_data(population_df, overwrite = TRUE)


population_df_merged <- read_csv("data-raw/population_data_merged.csv")

usethis::use_data(population_df_merged, overwrite = TRUE)


#Cost of Living
rpp <- read_csv("data-raw/rppmsa.csv", skip = 4, col_names = TRUE, na = c("", "(NA)"))

rpp$`2000` <- rpp$`2008`
rpp$`2005` <- rpp$`2008`
rpp$`2006` <- rpp$`2008`
rpp$`2007` <- rpp$`2008`
rpp$`2016` <- rpp$`2015`
rpp$`2017` <- rpp$`2015`

rpp <- rpp %>%
  filter(LineCode == 1) %>%
  select(-GeoName, -LineCode, -Description) %>%
  gather(2:15, key = year, value = "rpp") %>%
  mutate(year = as.numeric(year)) %>%
  rename(MSA = GeoFips) %>%
  add_FIPS_to_MSA() %>%
  pull_peers_FIPS() %>%
  select(FIPS, year, rpp)

COLA_df <- rpp

COLA_df %<>%
  group_by(year) %>%
  mutate(rpp_lou = rpp[FIPS == 21111],
         rpp_index = rpp_lou / rpp) %>%
  ungroup() %>%
  select(-rpp, -rpp_lou)

cpi <- read_csv("data-raw/c-cpi-u.csv", skip = 11)

cpi %<>%
  rename(year = Year) %>%
  gather(-year, key = month, value = cpi) %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 2000)

COLA_df %<>% left_join(cpi, by = "year")

COLA_df %<>%
  group_by(FIPS) %>%
  mutate(base_cpi = cpi[year == 2017],
         cpi_index = base_cpi/cpi) %>%
  ungroup() %>%
  select(-cpi, -base_cpi)

usethis::use_data(COLA_df, overwrite = TRUE)
