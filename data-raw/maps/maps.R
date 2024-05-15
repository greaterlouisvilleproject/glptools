library(dplyr)
library(readr)
library(stringr)
library(magrittr)
library(sf)
source("R/operators.R")
source("R/general_utils.R")

path <- "data-raw/maps/"

path_sf <- "data-raw/maps/shapefiles/"

# Crosswalks
nh_tract10        <- read_csv(path %p% "crosswalks/tract_to_nh.csv",                col_types = "cc")
ma_tract          <- read_csv(path %p% "crosswalks/tract_to_ma.csv",              col_types = "cc")
watterson_tract   <- read_csv(path %p% "crosswalks/tract_to_watterson.csv",       col_types = "cn")
west_lou_tract10  <- read_csv(path %p% "crosswalks/tract_to_west_louisville.csv", col_types = "cn")
west_lou_tract    <- read_csv(path %p% "crosswalks/tract_to_west_louisville_20.csv", col_types = "cn")
muw_tract10       <- read_csv(path %p% "crosswalks/Jefferson_Tract_Neighborhood.csv",    col_types = "cc",
                      skip = 2, col_names = TRUE)
muw_tract         <- read_csv(path %p% "crosswalks/TractsNew_2020.csv",
                            col_types = "ccnnnn",
                            col_names = TRUE)

attr(nh_tract10, 'spec') <- NULL
attr(ma_tract, 'spec') <- NULL
attr(watterson_tract, 'spec') <- NULL
attr(west_lou_tract10, 'spec') <- NULL
attr(west_lou_tract, 'spec') <- NULL
attr(muw_tract10, 'spec') <- NULL
attr(muw_tract, 'spec') <- NULL

ma_tract %<>% rename(market_area = `Market Area`)

muw_tract10 %<>%
  transmute(
    tract = "21111" %p% `Tract FIPS Code`,
    neighborhood = Neighborhood)

muw_tract %<>%
  transmute(
    tract = GEOID20,
    neighborhood = NEIGH1)

nh_tract <- nh_tract10 %>%
  left_join(tract10_tract20, by = c("tract" = "tract10")) %>%
  group_by(tract20, neighborhood) %>%
  reframe(percent = sum(percent)) %>%
  group_by(tract20) %>%
  filter(percent == max(percent)) %>%
  ungroup() %>%
  transmute(
    tract = tract20,
    neighborhood)

#Shape files

map_elementary  <- st_read(path_sf %p% "20192020_Elementary_Shapefile", quiet = TRUE)
#save(map_elementary, file = "../Projects/early-childhood/map_elementary.RData")

map_block_group10   <- st_read(path_sf %p% "block_group_2010",    quiet = TRUE)
map_block_group     <- st_read(path_sf %p% "block_group_2020",    quiet = TRUE)

map_tract_all00     <- st_read(path_sf %p% "tract_all_2000", quiet = TRUE)
map_tract_all10     <- st_read(path_sf %p% "tract_all_2010", quiet = TRUE)
map_tract_all       <- st_read(path_sf %p% "tract_all_2020", quiet = TRUE)

map_PUMA10          <- st_read(path_sf %p% "puma_2010",      quiet = TRUE)
map_PUMA            <- st_read(path_sf %p% "puma_2020",      quiet = TRUE)

map_county          <- st_read(path_sf %p% "county",         quiet = TRUE)

map_zip             <- st_read(path_sf %p% "zip",            quiet = TRUE)

map_district10      <- st_read(path_sf %p% "Council_Districts_2010", quiet = TRUE)
map_district        <- st_read(path_sf %p% "Council_Districts_2020", quiet = TRUE)

map_house           <- st_read(path_sf %p% "house",      quiet = TRUE)

map_senate          <- st_read(path_sf %p% "senate",      quiet = TRUE)

map_block_group10 %<>%
  filter(
    STATEFP == "21",
    COUNTYFP == "111") %>%
  st_transform(4326) %>%
  transmute(
    block_group = GEOID,
    tract = "21111" %p% TRACTCE,
    name = as.numeric(BLKGRPCE))

map_block_group %<>%
  filter(STATEFP == "21",
         COUNTYFP == "111") %>%
  st_transform(4326) %>%
  transmute(
    block_group = GEOID,
    tract = "21111" %p% TRACTCE,
    name = as.numeric(BLKGRPCE))

# 2000 maps
map_tract_all00 %<>%
  transmute(
    FIPS = paste0(STATEFP00, COUNTYFP00),
    tract = CTIDFP00) %>%
  filter(FIPS %in% FIPS_df_two_stl$FIPS) %>%
  st_transform(4326) %>%
  arrange(tract)

map_tract00 <- map_tract_all00 %>%
  filter(FIPS == "21111") %>%
  transmute(
    tract,
    name = as.numeric(str_sub(tract, 6, 9) %p% "." %p% str_sub(tract, 10, 11)))

# 2010 maps
map_tract_all10 %<>%
  transmute(
    FIPS = paste0(STATEFP, COUNTYFP),
    tract = GEOID)  %>%
  filter(FIPS %in% FIPS_df_two_stl$FIPS) %>%
  st_transform(4326) %>%
  arrange(tract)

map_tract10 <- map_tract_all10 %>%
  filter(FIPS == "21111") %>%
  transmute(
    tract,
    name = as.numeric(str_sub(tract, 6, 9) %p% "." %p% str_sub(tract, 10, 11)))

# 2020 maps
map_tract_all %<>%
  transmute(
    FIPS = paste0(STATEFP, COUNTYFP),
    tract = GEOID)  %>%
  filter(FIPS %in% FIPS_df_two_stl$FIPS) %>%
  st_transform(4326) %>%
  arrange(tract)

map_tract <- map_tract_all %>%
  filter(FIPS == "21111") %>%
  left_join(nh_tract, by = "tract") %>%
  transmute(
    tract,
    name = as.numeric(str_sub(tract, 6, 9) %p% "." %p% str_sub(tract, 10, 11)),
    neighborhood)

# Summarize neighborhood areas
map_nh <- map_tract %>%
  group_by(neighborhood) %>%
  summarise(.groups = "drop")

map_muw <- map_tract %>%
  select(-neighborhood) %>%
  left_join(muw_tract, by =  "tract") %>%
  group_by(neighborhood) %>%
  summarise(.groups = "drop")

map_market <- map_tract %>%
  left_join(ma_tract, by =  "tract") %>%
  group_by(market_area) %>%
  summarise(.groups = "drop")

# Load other kinds of maps

map_PUMA10 %<>%
  transmute(
    STATEFIP,
    PUMA = as.numeric(PUMA)) %>%
  filter(STATEFIP == "21", PUMA %in% 1701:1706) %>%
  st_transform(4326)

map_PUMA %<>%
  transmute(
    STATEFIP = STATEFP20,
    PUMA = as.numeric(PUMACE20)) %>%
  filter(STATEFIP == "21", PUMA %in% 1701:1706) %>%
  st_transform(4326)

map_zip %<>%
  st_transform(4326) %>%
  transmute(
    zip = ZIPCODE)

map_county %<>%
  transmute(
    FIPS = STATEFP %p% COUNTYFP,
    county = NAME)

map_county_peers <- map_county %>%
  filter(FIPS %in% FIPS_df$FIPS) %>%
  st_transform(4326)

map_msa_lou <- map_county %>%
  filter(FIPS %in% MSA_FIPS$FIPS[MSA_FIPS$MSA == "31140"]) %>%
  st_transform(4326)

map_house %<>%
  filter(STATEFP == "21") %>%
  transmute(
    STATEFIP = STATEFP,
    house_district = as.numeric(SLDLST)) %>%
  st_transform(4326)

map_senate %<>%
  filter(STATEFP == "21") %>%
  transmute(
    STATEFIP = STATEFP,
    senate_district = as.numeric(SLDUST)) %>%
  st_transform(4326)

map_district10 %<>%
  st_transform(4326) %>%
  transmute(district = coundist)

map_district %<>%
  st_transform(4326) %>%
  transmute(district = COUNDIST)

# Previous data

update_sysdata(
  "nh_tract10", "ma_tract", "watterson_tract", "west_lou_tract10", "muw_tract10",
  "map_tract_all00", "map_tract_all10",
  "map_tract00", "map_tract10",
  "map_block_group10",
  "map_PUMA10",
  "map_market",
  "map_district10")

usethis::use_data(
  nh_tract, west_lou_tract, muw_tract, map_tract_all, map_tract, map_block_group,
  map_nh, map_muw,
  map_PUMA,
  map_zip,
  map_county_peers, map_msa_lou,
  map_house, map_senate, map_district,
  overwrite = TRUE)



