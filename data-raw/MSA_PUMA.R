library(readr)
library(dplyr)
library(magrittr)
source("R/general.R")

path <- "data-raw/MSA_PUMA/"

# 2000 to 2011

MSA_PUMA_00 <- read_csv(path %p% "MSA2013_PUMA2000_pop2010_crosswalk.csv", col_types = "ccncnnnnnn")

MSA_PUMA_00 %<>%
  select(
    MSA = `MSA Code`,
    STATEFIP = `State FIPS Code`,
    PUMA = `2000 PUMA Code`,
    pct_pop = `Percent PUMA Population`)

MSA_PUMA_00 <- data.frame(
  MSA_PUMA_00,
  year = rep(2000:2011, each = nrow(MSA_PUMA_00)))

# 2012 and on

MSA_PUMA_10 <- read_csv(path %p% "MSA2013_PUMA2010_crosswalk.csv", col_types = "ccncncnnnnn")

MSA_PUMA_10 %<>%
  select(
    MSA = `MSA Code`,
    STATEFIP = `State FIPS Code`,
    PUMA = `PUMA Code`,
    pct_pop = `Percent PUMA Population`)

MSA_PUMA_10 <- data.frame(
  MSA_PUMA_10,
  year = rep(2012:2017, each = nrow(MSA_PUMA_10)))

MSA_PUMA <- bind_rows(MSA_PUMA_00, MSA_PUMA_10)

MSA_PUMA %<>%
  filter(
    MSA %in% MSA_df$MSA,
    pct_pop >= 50)

MSA_PUMA %<>%
  select(MSA, year, STATEFIP, PUMA)

usethis::use_data(MSA_PUMA, overwrite = TRUE)


