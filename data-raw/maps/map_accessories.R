library(dplyr)
library(readr)
library(stringr)
library(magrittr)
library(sf)
source("R/operators.R")

path <- "data-raw/maps/shapefiles/"

# Expressways

expressways <- st_read(path %p% "Louisville_Metro_Area_KY_Major_Roads", quiet = TRUE)

expressways %<>%
  st_transform(4326) %>%
  filter(
    COUNTY_NAM == "JEFFERSON",
    str_detect(ROAD_NAME, "64|65|71|RIVERSIDE|SNYDER"),
    str_detect(ROAD_NAME, "RAMP|WEST|NORTH", negate = TRUE))

expressways_line <- expressways %>%
  summarize() %>%
  st_cast("LINESTRING") %>%
  smoothr::smooth(method = "ksmooth", smoothness = 10)
  #rmapshaper::ms_simplify()

expressways_buffer <- expressways_line %>%
  st_buffer(100, endCapStyle = "FLAT", joinStyle = "MITRE") %>%
  summarize()

# Ohio River

ohio <- st_read("data-raw/maps/shapefiles/county", quiet = TRUE)

# Keep just counties around Louisville
ohio %<>%
  st_transform(4326) %>%
  transmute(
    FIPS = STATEFP %p% COUNTYFP,
    county = NAME)

ohio %<>%
  filter(FIPS %in% c(18025, 18061, 18043, 18019, 18077, 21163, 21093, 21111, 21185, 21223))

# Find borders for the combinations of counties that are adjacent to the Ohio.
# There an issue with combination #4, so try snapping borders first.

r1 = cartography::getBorders(filter(ohio, FIPS == 18025 | FIPS == 21163))

r1_1 = filter(map_county, FIPS %in% c(18025, 21163)) %>%  st_transform(3857)
r1_2 = filter(map_county, FIPS == 21163) %>%  st_transform(3857)
r1_corrected = st_snap(r1_1, r1_2, 10)
r1 = cartography::getBorders(r1_corrected)
st_crs(r1) = 3857
r1 %<>% st_transform(4326)

r2 = cartography::getBorders(filter(map_county, FIPS == 18061 | FIPS == 21163))
r3 = cartography::getBorders(filter(map_county, FIPS == 18061 | FIPS == 21093))
r4 = cartography::getBorders(filter(map_county, FIPS == 18061 | FIPS == 21111))

st_crs(r2) <- 4326
st_crs(r3) <- 4326
st_crs(r4) <- 4326

r5_1 = filter(map_county, FIPS %in% c(18043, 21111)) %>%  st_transform(3857)
r5_2 = filter(map_county, FIPS == 21111) %>%  st_transform(3857)
r5_corrected = st_snap(r5_1, r5_2, 10)
r5 = cartography::getBorders(r5_corrected)
st_crs(r5) = 3857
r5 %<>% st_transform(4326)

r6 = cartography::getBorders(filter(map_county, FIPS == 18019 | FIPS == 21111))
r7 = cartography::getBorders(filter(map_county, FIPS == 18019 | FIPS == 21185))
r8 = cartography::getBorders(filter(map_county, FIPS == 18019 | FIPS == 21223))
r9 = cartography::getBorders(filter(map_county, FIPS == 18077 | FIPS == 21223))

st_crs(r6) <- 4326
st_crs(r7) <- 4326
st_crs(r8) <- 4326
st_crs(r9) <- 4326

# Combine points and simplify
ohio <- bind_rows(r1, r2, r3, r4, r5, r6, r7, r8, r9)

ohio_line_msa <- ohio %>%
  summarize() %>%
  st_cast("LINESTRING") %>%
  #rmapshaper::ms_simplify() %>%
  smoothr::smooth(method = "ksmooth", smoothness = 10)

ohio_line <- ohio_line_msa %>%
  st_crop(xmin = -86.01, xmax = -85, ymin = 37.8, ymax = 38.41)

ohio_buffer_msa <- ohio_line_msa %>%
  st_buffer(100, endCapStyle = "FLAT", joinStyle = "MITRE") %>%
  summarize()

ohio_buffer <- ohio_line %>%
  st_buffer(100, endCapStyle = "FLAT", joinStyle = "MITRE") %>%
  summarize()

# Council District Points

sf_use_s2(FALSE)

buffers <- c()

for(d in 1:26) {
  this_buffer = 0
  this_step = -0.005
  current_area = 10001
  step = 1

  #g <- ggplot()

  while(current_area > 10000){

    #browser()
    # Buffer inside the polygon using this_buffer
    temp_sf <- st_buffer(map_district[d,], dist = this_buffer, singleSide = T)

    # Calculate area of polygon
    this_area = st_area(temp_sf) %>% as.numeric()

    # If remaining area > 0, enlarge buffer by subtracting and going away from zero.
    #   Also record most recent correct buffer and area produced by buffer.
    # If remaining area is 0, reduce buffer by going toward 0.
    if (this_area > 0) {
      current_result = this_buffer
      current_area = this_area

      this_buffer = this_buffer + this_step

      #g <- g + geom_sf(data = temp_sf, fill = viridis::viridis(10)[step])

    } else {
      this_buffer = this_buffer - this_step

      # Cut search step in half
      this_step = this_step / 2
    }

    step = step + 1

    print(paste0(d, ": ", this_buffer))
    print(this_area)

  }

  #print(g)

  buffers <- c(buffers, current_result)

}

buffered_map <- st_buffer(map_district, dist = buffers, singleSide = T)

district_label_points <- st_point_on_surface(buffered_map)

usethis::use_data(expressways_line, expressways_buffer,
                  ohio_line_msa, ohio_buffer_msa,
                  ohio_line, ohio_buffer,
                  district_label_points,
                  overwrite = TRUE)

