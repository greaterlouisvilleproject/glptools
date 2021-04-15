library(glptools)
glp_load_packages(T)
library(sf)
library(ipumsr)

path <- "data-raw/council_tract/"

### CREATE BLOCK GROUP TO DISTRICT CROSSWALK
map_block <- st_read(path %p% "nhgis0009_shape", quiet = TRUE)
block_population <- read_nhgis(path %p% "nhgis0009_csv.zip")

# Project using meters
dist_proj <- st_transform(map_district, 3857)
tract_proj <- st_transform(map_tract, 3857)
bg_proj   <- st_transform(map_block_group, 3857)
block_proj <- st_transform(map_block, 3857)

# Create block population
block_proj %<>%
  filter(COUNTYFP10 == "111") %>%
  st_make_valid() %>%
  left_join(block_population, by = "GISJOIN") %>%
  transmute(
    tract = paste0(STATEFP10, COUNTYFP10, TRACTCE10),
    block_group = paste0(tract, str_sub(BLOCKCE10, 1, 1)),
    block = paste0(tract, BLOCKCE10),
    population = H7V001)

block_population %<>%
  transmute(
    tract = paste0(STATEA, COUNTYA, TRACTA),
    block_group = paste0(tract, str_sub(BLKGRPA, 1, 1)),
    block = paste0(tract, BLOCKA),
    population = H7V001)

#INTERSECT WITH METRO COUNCIL DISTRICTS
if("block_processed.RData" %in% list.files(path)) {
  load(path %p% "block_processed.RData")
} else {
  block_snap <- st_snap(block_proj, dist_proj, tolerance = 3.3) %>%
    st_make_valid()

  dist_block <- st_intersection(dist_proj, block_snap)

  save(dist_block, block_snap, file = path %p% "block_processed.RData")
}

# dist_bg1 <- dist_block %>%
#   st_collection_extract("POLYGON") %>%
#   rmapshaper::ms_simplify(keep = .25)

district_block1 <- dist_block %>%
  mutate(area    = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(block) %>%
  transmute(
    district,
    tract,
    block_group,
    area,
    block_in_district = as.numeric(area / sum(area) * 100)) %>%
  ungroup()

district_block2 <- district_block1 %>%
  left_join(block_population, by = c("tract", "block_group", "block")) %>%
  mutate(across(population, ~ . * block_in_district / 100)) %>%
  mutate(population = round(population, 1)) %>%
  filter(population >= 1)

# Recalculate percent and reallocate population
district_block2 %<>%
  group_by(block) %>%
  transmute(
    district, tract, block_group, block,
    block_in_district = as.numeric(area / sum(area) * 100)) %>%
  ungroup() %>%
  left_join(block_population, by = c("tract", "block_group", "block")) %>%
  mutate(across(population, ~ . * block_in_district / 100)) %>%
  mutate(population = round(population, 1))

# Summarize at the tract level
district_block_group <- district_block2 %>%
  group_by(district, tract, block_group) %>%
  summarise(across(population, ~sum(.)), .groups = "drop")

district_tract <- district_block2 %>%
  group_by(district, tract) %>%
  summarise(across(population, ~sum(.)), .groups = "drop")

usethis::use_data(district_block_group, district_tract, overwrite = TRUE)

if (FALSE) {
  # COMBINE WITH POPULATION DATA
  block_group_population <- read_nhgis(path %p% "nhgis0008_csv.zip")

  block_group_population %<>%
    filter(COUNTYA == "111") %>%
    transmute(
      tract = paste0("21", COUNTYA, TRACTA),
      block_group = tract %p% BLKGRPA,

      total = AJWBE001,
      male = AJWBE002,
      female = AJWBE026,

      age_0_18 =
        AJWBE003 + AJWBE004 + AJWBE005 + AJWBE006 +
        AJWBE027 + AJWBE028 + AJWBE029 + AJWBE030,
      age_25_64 =
        AJWBE011 + AJWBE012 + AJWBE013 + AJWBE014 + AJWBE015 + AJWBE016 + AJWBE017 + AJWBE018 + AJWBE019 +
        AJWBE035 + AJWBE036 + AJWBE037 + AJWBE038 + AJWBE039 + AJWBE040 + AJWBE041 + AJWBE042 + AJWBE043,

      white    = AJWVE003,
      black    = AJWVE004,
      hispanic = AJWVE012,
      asian    = AJWVE006)

  # Add area-based crosswalk and project population in each district.
  # Remove less than 1 resident is calculated to exist.
  district_block_group %<>%
    left_join(block_group_population, by = c("tract", "block_group")) %>%
    mutate(across(total:asian, ~ . * block_group_in_district / 100)) %>%
    filter(total >= 1)

  # Recalculate percent and reallocate population
  district_block_group %<>%
    group_by(block_group) %>%
    transmute(
      district, tract, block_group,
      block_group_in_district = as.numeric(area / sum(area) * 100)) %>%
    ungroup() %>%
    left_join(block_group_population, by = c("tract", "block_group")) %>%
    mutate(across(total:asian, ~ . * block_group_in_district / 100))

  # Summarise at the tract level
  district_tract <- district_block_group %>%
    group_by(district, tract) %>%
    summarise(across(total:asian, ~sum(.)), .groups = "drop")

  usethis::use_data(district_block_group, district_tract, overwrite = TRUE)
}

if(FALSE){
  tract_proj   <- st_transform(map_tract, 3857)

  tract_snap <- st_snap(tract_proj, dist_proj, tolerance = 10) %>%
    st_make_valid()

  dist_tract <- st_intersection(dist_proj, tract_snap)

  dist_tract %<>%
    st_collection_extract("POLYGON") %>%
    rmapshaper::ms_simplify(keep = .25)

  district_tract <- dist_tract %>%
    mutate(area    = st_area(.)) %>%
    st_drop_geometry() %>%
    group_by(tract) %>%
    transmute(
      district = coundist,
      tract,
      area,
      tract_in_district = as.numeric(area / sum(area) * 100)) %>%
    ungroup()

  tract_population <- block_group_population %>%
    group_by(tract) %>%
    summarise(across(total:asian, sum), .groups = "drop")

  district_tract %<>% left_join(tract_population, by = "tract")
}
