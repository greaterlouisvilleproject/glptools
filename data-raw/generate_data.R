# GLP FIPS and MSA peers
source("data-raw/basic_info.R")

# MSA to FIPS crosswalk
source("data-raw/MSA_FIPS.R")

# MSA and FIPS to PUMA crosswalks
source("data-raw/MSA_PUMA.R")
source("data-raw/FIPS_PUMA.R")

# MSA and FIPS to zip code crosswalks
source("data-raw/zip_codes.R")

# Inflation and Cost of Living adjustment data frame
source("data-raw/inflation_and_COLA.R")

# Population data frames
source("data-raw/population.R")

# Maps
source("data-raw/maps.R")
