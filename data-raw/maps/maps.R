library(dplyr)
library(readr)
library(stringr)
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

west_lou_tract <- read_csv(path %p% "crosswalks/tract_to_west_louisville.csv", col_types = "cn")
attr(watterson_tract, 'spec') <- NULL

muw_tract <- read_csv(path %p% "MUW/Jefferson_Tract_Neighborhood.csv",
                      skip = 2, col_names = TRUE, col_types = "cc")
attr(muw_tract, 'spec') <- NULL

muw_tract %<>%
  transmute(
    tract = "21111" %p% `Tract FIPS Code`,
    neighborhood = Neighborhood)


#Shapefiles

map_block_group <- readOGR(path %p% "block_group", layer = "tl_2018_21_bg",
                           GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_tract <- readOGR(path %p% "tract_2010", layer = "tract",
                     GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_tract_2000 <- readOGR(path %p% "tract_2000", layer = "tr21_d00",
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

map_PUMA <- readOGR(path %p% "puma_2010", layer = "ipums_puma_2010",
                    GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_PUMA@data %<>%
  transmute(
    STATEFIP,
    PUMA = as.numeric(PUMA))

map_PUMA <- map_PUMA[map_PUMA@data$STATEFIP == "21" &
                     map_PUMA@data$PUMA %in% 1701:1706,]

map_PUMA %<>% spTransform(map_tract@proj4string@projargs)

# 2010 tracts
map_tract_all_10 <- readOGR(path %p% "tract_all_2010", layer = "US_tract_2018",
                            GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_tract_all_10@data %<>%
  transmute(
    FIPS = paste0(STATEFP, COUNTYFP),
    tract = GEOID)

map_tract_all_10 <- map_tract_all_10[map_tract_all_10@data$FIPS %in% FIPS_df_two_stl$FIPS,]

map_tract_all_10 <- map_tract_all_10[order(map_tract_all_10@data$tract),]

# 2000 tracts
map_tract_all_00 <- readOGR(path %p% "tract_all_2000", layer = "US_tract10_2000",
                            GDAL1_integer64_policy = TRUE, stringsAsFactors = FALSE, verbose = FALSE)

map_tract_all_00@data %<>%
  transmute(
    FIPS = paste0(STATEFP00, COUNTYFP00),
    tract = CTIDFP00)

map_tract_all_00 <- map_tract_all_00[map_tract_all_00@data$FIPS %in% FIPS_df_two_stl$FIPS,]

map_tract_all_00 <- map_tract_all_00[order(map_tract_all_00@data$tract),]

# Check if ordered
#mean(map_tract_all_00@data$tract[order(map_tract_all_00@data$tract)] == map_tract_all_00@data$tract)

# Create MUW map
map_block_group <- map_block_group[map_block_group@data$COUNTYFP == "111",]
map_block_group@data %<>%
  transmute(
    block_group = GEOID,
    tract = "21111" %p% TRACTCE,
    name = as.numeric(BLKGRPCE))

map_tract@data %<>%
  transmute(
    tract = str_sub(GEO_ID, -11),
    name = as.numeric(NAME)) %>%
  left_join(nh_tract, by = "tract")


map_muw <- map_tract

map_muw@data %<>%
  select(-neighborhood) %>%
  left_join(muw_tract, by =  "tract")

row.names(map_muw) <- row.names(map_muw@data)
map_muw <- spChFIDs(map_muw, row.names(map_muw))

map_muw <- rgeos::gUnaryUnion(map_muw, id = map_muw@data$neighborhood)

nh_names <- row.names(map_muw)

nh_names <- data.frame(neighborhood = nh_names, row.names = nh_names, stringsAsFactors = FALSE)

map_muw <- SpatialPolygonsDataFrame(map_muw, nh_names)


map_tract_2000 <- map_tract_2000[map_tract_2000@data$COUNTY == "111",]
map_tract_2000@data %<>%
  transmute(
    tract = TRACT,
    name = as.numeric(NAME))

map_zip@data %<>%
  transmute(
    zip = ZIPCODE)

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

usethis::use_data(nh_tract, ma_tract, watterson_tract, west_lou_tract, muw_tract,
                  map_tract_all_00, map_tract_all_10,
                  map_tract, map_nh, map_muw, map_PUMA,
                  map_block_group, map_tract_2000, map_zip, map_market, map_county,
                  overwrite = TRUE)
