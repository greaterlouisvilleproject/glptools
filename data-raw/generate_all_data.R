# GLP FIPS and MSA peers
source("data-raw/basic_info/basic_info.R")

# MSA to FIPS crosswalk
source("data-raw/MSA_FIPS/MSA_FIPS.R")

# MSA and FIPS to PUMA crosswalks
source("data-raw/PUMA_crosswalk/PUMA_crosswalk.R")

# MSA and FIPS to zip code crosswalks
source("data-raw/zip_codes/zip_codes.R")

# Tract 2000 to tract 2010 crosswalks
source("data-raw/tract_crosswalk/tract_crosswalk.R")

# Inflation and Cost of Living adjustment data frame
source("data-raw/inflation_and_COLA/inflation_and_COLA.R")

# Maps
source("data-raw/maps/maps.R")

# Map Accessories
source("data-raw/maps/map_accessories.R")

# Metro Council Map and crosswalk (long code gets its own file)
#source("data-raw/council_tract/council_tract.R")
