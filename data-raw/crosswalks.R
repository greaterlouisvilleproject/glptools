library(tidyverse)
library(magrittr)

tract_nh <- read_csv("data-raw/crosswalks/tract_to_nh.csv")

usethis::use_data(tract_nh, overwrite = TRUE)

FIPS_names <- read_csv("data-raw/FIPS_names.csv")

usethis::use_data(FIPS_names, overwrite = TRUE)

MSA_names <- read_csv("data-raw/MSA_names.csv")

usethis::use_data(MSA_names, overwrite = TRUE)


MSA_FIPS <- read_csv("data-raw/MSA_definitions.csv")

MSA_FIPS %<>%
  transmute(
    FIPS = as.character(`FIPS State county code`),
    MSA = `CBSA (Blanks are Rural)`) %>%
  right_join(MSA_names, by = "MSA") %>%
  select(-city) 

MSA_FIPS %<>%
  rbind(c("MERGED", 41180)) %>%
  mutate(MSA = as.numeric(MSA)) %>%
  select(MSA, FIPS)

usethis::use_data(MSA_FIPS, overwrite = TRUE)



MSA_PUMA_00 <- read_csv("data-raw/MSA2013_PUMA2000_pop2010_crosswalk.csv")
MSA_PUMA_10 <- read_csv("data-raw/MSA2013_PUMA2010_crosswalk.csv")

MSA_PUMA_00 %<>%
  transmute(
    MSA = `MSA Code`,
    STATEFIP = `State FIPS Code`,
    PUMA = `2000 PUMA Code`,
    pct_pop = `Percent PUMA Population`)

MSA_PUMA_00 <- data.frame(
  MSA_PUMA_00, 
  year = rep(2000:2011, each = nrow(MSA_PUMA_00)))

MSA_PUMA_10 %<>%
  transmute(
    MSA = `MSA Code`,
    STATEFIP = as.numeric(`State FIPS Code`),
    PUMA = as.numeric(`PUMA Code`),
    pct_pop = `Percent PUMA Population`)

MSA_PUMA_10 <- data.frame(
  MSA_PUMA_10, 
  year = rep(2012:2017, each = nrow(MSA_PUMA_10)))

MSA_PUMA <- bind_rows(MSA_PUMA_00, MSA_PUMA_10)

MSA_PUMA %<>%
  filter(
    MSA %in% MSA_names$MSA,
    pct_pop >= 50) %>%
  select(-pct_pop)

MSA_PUMA %<>% 
  select(MSA, year, STATEFIP, PUMA)

usethis::use_data(MSA_PUMA, overwrite = TRUE)
