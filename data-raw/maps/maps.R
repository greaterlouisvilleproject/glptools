library(dplyr)
library(readr)
library(stringr)
library(magrittr)
library(sf)
source("R/operators.R")

path <- "data-raw/maps/"

# Crosswalks
nh_tract        <- read_csv(path %p% "crosswalks/tract_to_nh.csv",              col_types = "cc")
ma_tract        <- read_csv(path %p% "crosswalks/tract_to_ma.csv",              col_types = "cc")
watterson_tract <- read_csv(path %p% "crosswalks/tract_to_watterson.csv",       col_types = "cn")
west_lou_tract  <- read_csv(path %p% "crosswalks/tract_to_west_louisville.csv", col_types = "cn")
muw_tract       <- read_csv(path %p% "MUW/Jefferson_Tract_Neighborhood.csv",    col_types = "cc",
                      skip = 2, col_names = TRUE)

attr(nh_tract, 'spec') <- NULL
attr(ma_tract, 'spec') <- NULL
attr(watterson_tract, 'spec') <- NULL
attr(watterson_tract, 'spec') <- NULL
attr(muw_tract, 'spec') <- NULL

ma_tract %<>% rename(market_area = `Market Area`)

muw_tract %<>%
  transmute(
    tract = "21111" %p% `Tract FIPS Code`,
    neighborhood = Neighborhood)


#Shape files

map_elementary  <- st_read(path %p% "20192020_Elementary_Shapefile", quiet = TRUE)
save(map_elementary, file = "../Projects/early-childhood/map_elementary.RData")

map_block_group  <- st_read(path %p% "block_group",    quiet = TRUE)
map_tract        <- st_read(path %p% "tract_2010",     quiet = TRUE)
map_tract_all_00 <- st_read(path %p% "tract_all_2000", quiet = TRUE)
map_tract_all_10 <- st_read(path %p% "tract_all_2010", quiet = TRUE)
map_nh           <- st_read(path %p% "neighborhood",   quiet = TRUE)
map_zip          <- st_read(path %p% "zip",            quiet = TRUE)
map_market       <- st_read(path %p% "market area",    quiet = TRUE)
map_county       <- st_read(path %p% "county",         quiet = TRUE)
map_PUMA         <- st_read(path %p% "puma_2010",      quiet = TRUE)
map_district     <- st_read("data-raw/council_tract/Council_Districts", quiet = TRUE)

map_block_group %<>%
  st_transform(4326) %>%
  filter(COUNTYFP == "111") %>%
  transmute(
    block_group = GEOID,
    tract = "21111" %p% TRACTCE,
    name = as.numeric(BLKGRPCE))

map_tract %<>%
  st_transform(4326) %>%
  transmute(
    tract = str_sub(GEO_ID, -11),
    name = as.numeric(NAME)) %>%
  left_join(nh_tract, by = "tract")

map_tract_all_00 %<>%
  st_transform(4326) %>%
  transmute(
    FIPS = paste0(STATEFP00, COUNTYFP00),
    tract = CTIDFP00) %>%
  filter(FIPS %in% FIPS_df_two_stl$FIPS) %>%
  arrange(tract)

map_tract_all_10 %<>%
  st_transform(4326) %>%
  transmute(
    FIPS = paste0(STATEFP, COUNTYFP),
    tract = GEOID)  %>%
  filter(FIPS %in% FIPS_df_two_stl$FIPS) %>%
  arrange(tract)

map_PUMA %<>%
  st_transform(4326) %>%
  transmute(
    STATEFIP,
    PUMA = as.numeric(PUMA)) %>%
  filter(STATEFIP == "21", PUMA %in% 1701:1706)

map_muw <- map_tract %>%
  st_transform(4326) %>%
  select(-neighborhood) %>%
  left_join(muw_tract, by =  "tract") %>%
  group_by(neighborhood) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

map_zip %<>%
  st_transform(4326) %>%
  transmute(
    zip = ZIPCODE)

map_nh %<>%
  st_transform(4326) %>%
  transmute(
    neighborhood = Neighborho)

map_market %<>%
  st_transform(4326) %>%
  transmute(
    market = Area)

map_county %<>%
  st_transform(4326) %>%
  transmute(
    FIPS = FIPS,
    county = NAME)

# Derive 2000 tracts for just Louisville
map_tract_2000 <- map_tract_all_00 %>%
  st_transform(4326) %>%
  filter(FIPS == "21111") %>%
  transmute(
    tract,
    name = as.numeric(str_sub(tract, 6, 11)) / 100)

map_district %<>%
  st_transform(4326) %>%
  transmute(district = coundist)

usethis::use_data(nh_tract, ma_tract, watterson_tract, west_lou_tract, muw_tract,
                  map_tract_all_00, map_tract_all_10,
                  map_tract, map_nh, map_muw, map_PUMA,
                  map_block_group, map_tract_2000, map_zip, map_market, map_county,
                  overwrite = TRUE)
