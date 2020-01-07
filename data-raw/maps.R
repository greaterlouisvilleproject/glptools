library(dplyr)
library(readr)
library(magrittr)
library(rgdal)

path <- "data-raw/maps/"

# Crosswalks

nh_tract <- read_csv(path %p% "crosswalks/tract_to_nh.csv", col_types = "cc")
attr(nh_tract, 'spec') <- NULL

ma_tract <- read_csv(path %p% "crosswalks/tract_to_ma.csv", col_types = "cc")
attr(ma_tract, 'spec') <- NULL

ma_tract %<>% rename(market_area = `Market Area`)

watterson_tract <- read_csv(path %p% "crosswalks/tract_to_watterson.csv", col_types = "cn")
attr(watterson_tract, 'spec') <- NULL

muw_tract <- read_csv(path %p% "MUW/Jefferson_Tract_Neighborhood.csv",
                      skip = 2, col_names = TRUE, col_types = "cc")
attr(muw_tract, 'spec') <- NULL

muw_tract %<>%
  transmute(
    GEO_ID = "1400000US21111" %p% `Tract FIPS Code`,
    neighborhood = Neighborhood)


#Shapefiles

map_block_group <- readOGR(path %p% "block_group", layer = "tl_2018_21_bg",
                           GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_tract <- readOGR(path %p% "tract", layer = "tract",
                     GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_tract_2000 <- readOGR(path %p% "tract2000", layer = "tr21_d00",
                          GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_nh <- readOGR(path %p% "neighborhood", layer = "neighborhood",
                  GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

suppressWarnings(
map_zip <- readOGR(path %p% "zip", layer = "Jefferson_County_KY_ZIP_Codes",
                   GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)
)

map_market <- readOGR(path %p% "market area", layer = "market area",
                      GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_county <- readOGR(path %p% "county", layer = "cb_2017_us_county_5m",
                      GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

# Create MUW map
map_muw <- map_tract

map_muw@data %<>% left_join(muw_tract, by = "GEO_ID")

row.names(map_muw) <- row.names(map_muw@data)
map_muw <- spChFIDs(map_muw, row.names(map_muw))

map_muw <- rgeos::gUnaryUnion(map_muw, id = map_muw@data$neighborhood)

nh_names <- row.names(map_muw)

nh_names <- data.frame(neighborhood = nh_names, row.names = nh_names, stringsAsFactors = FALSE)

map_muw <- SpatialPolygonsDataFrame(map_muw, nh_names)


map_block_group <- map_block_group[map_block_group@data$COUNTYFP == "111",]
map_block_group@data %<>%
  transmute(
    GEO_ID = GEOID,
    tract = TRACTCE,
    block_group = as.numeric(BLKGRPCE))

map_tract@data %<>%
  left_join(nh_tract, by = "GEO_ID") %>%
  transmute(
    GEO_ID,
    tract = as.numeric(NAME),
    neighborhood)

map_tract_2000 <- map_tract_2000[map_tract_2000@data$COUNTY == "111",]
map_tract_2000@data %<>%
  transmute(
    tract = NAME)

map_zip@data %<>%
  transmute(
    zip = as.numeric(ZIPCODE))

map_nh@data %<>%
  transmute(
    neighborhood = Neighborho)

map_market@data %<>%
  transmute(
    market = Area)

map_county@data %<>%
  transmute(
    FIPS = FIPS,
    county = NAME)

usethis::use_data(nh_tract, ma_tract, watterson_tract, muw_tract,
                  map_block_group, map_tract_2000, map_zip, map_market, map_county,
                  overwrite = TRUE)

# Publicly visible versions are written by glpdata
usethis::use_data(map_tract, map_nh, map_muw, overwrite = TRUE, internal = TRUE)
