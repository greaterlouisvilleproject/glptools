library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
source("R/operators.R")
source("R/general_utils.R")
source("R/peer_utils.R")
source("R/glp_utils.R")

path <- "data-raw/PUMA_crosswalk/"

MSA_FIPS_12 <- MSA_FIPS2012 %>% rename(MSA_12 = MSA)

# 2000 to 2011
# Data from the Michigan State University Population Studies Center
# https://www.psc.isr.umich.edu/dis/census/Features/puma2cnty/ -> PUMA to County Crosswalk [p2c] -> Puma to County Crosswalk

PUMA00 <- read_fwf(path %p% "puma2county.txt",
                    fwf_positions(start = c(1, 4, 9,  13, 21, 25, 34, 43, 50, 58, 66),
                                  end   = c(2, 8, 12, 17, 22, 31, 40, 49, 57, 65, NA),
                                  col_names = c("STATEFIP", "PUMA", "countyFIPS",	"MSA", "PMSA", "PUMApop",
                                                "FIPSPop", "PUMApopinFIPS", "PctPUMAinFIPS", "PctFIPSinPUMA", "Name")),
                    col_types = "cncccnnnnnc")

# Create FIPS codes from State and County FIPS, then subset to peers.
PUMA00 %<>%
  mutate(
    STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = "0"),
    countyFIPS = str_pad(countyFIPS, width = 3, side = "left", pad = "0"),
    FIPS = paste0(STATEFIP, countyFIPS)) %>%
  select(STATEFIP, FIPS, PUMA, PctPUMAinFIPS)

# Calculate percent in MSA
PUMA00 %<>%
  left_join(MSA_FIPS,    by = "FIPS") %>%
  group_by(MSA, STATEFIP, PUMA) %>%
  mutate(PctPUMAinMSA = sum(PctPUMAinFIPS)) %>%
  ungroup() %>%
  left_join(MSA_FIPS_12, by = "FIPS") %>%
  group_by(MSA_12, STATEFIP, PUMA) %>%
  mutate(PctPUMAinMSA_12 = sum(PctPUMAinFIPS)) %>%
  ungroup()

MSA_PUMA00 <- PUMA00 %>%
  filter(PctPUMAinMSA > 0.5) %>%
  transmute(MSA, STATEFIP, PUMA) %>%
  expand(nesting(MSA, STATEFIP, PUMA), year = c(2000, 2005:2011))

MSA_PUMA00_12 <- PUMA00 %>%
  filter(PctPUMAinMSA_12 > 0.5) %>%
  transmute(MSA = MSA_12, STATEFIP, PUMA) %>%
  expand(nesting(MSA, STATEFIP, PUMA), year = c(2000, 2005:2011))

FIPS_PUMA00 <- PUMA00 %>%
  filter(PctPUMAinFIPS > 0.5) %>%
  transmute(FIPS, STATEFIP, PUMA) %>%
  expand(nesting(FIPS, STATEFIP, PUMA), year = c(2000, 2005:2011))

# 2012 to 2022
# Data from the Census Bureau's FTP website: https://www2.census.gov/geo/docs/reference/puma/

puma_2010_files <- list.files(path %p% "PUMA_FIPS_10")

for(f in puma_2010_files){
  df <- read_fwf(paste0(path, "PUMA_FIPS_10", "/", f),
                 fwf_cols(SL = 3, STATEFIP = 2, STATENS = 8, PUMA = 5,
                          COUNTYFIPS = 3, COUNTYns = 8, COUSUBFP = 5,
                          COUSUBNS = 8, PLACEFP = 5, PLACENS = 8, TRACTCE = 6,
                          pop = 9, HU10 = 9, NAME = 100),
                 col_types = "ccccccnncccnnc")

  # Create FIPS codes from State and County FIPS, then subset.
  df %<>%
    filter(SL == "796") %>%
    mutate(
      FIPS = paste0(STATEFIP, COUNTYFIPS),
      PUMA = as.numeric(PUMA),
      STATEFIP) %>%
    group_by(PUMA) %>%
    mutate(PctPUMAinFIPS = pop / sum(pop)) %>%
    select(STATEFIP, PUMA, FIPS, PctPUMAinFIPS) %>%
    ungroup()

  PUMA10 <- assign_row_join(PUMA10, df)
}

PUMA10 %<>%
  left_join(MSA_FIPS, by = "FIPS") %>%
  group_by(MSA, STATEFIP, PUMA) %>%
  mutate(PctPUMAinMSA = sum(PctPUMAinFIPS)) %>%
  ungroup() %>%
  left_join(MSA_FIPS_12, by = "FIPS") %>%
  group_by(MSA_12, STATEFIP, PUMA) %>%
  mutate(PctPUMAinMSA_12 = sum(PctPUMAinFIPS)) %>%
  ungroup()

MSA_PUMA10 <- PUMA10 %>%
  filter(PctPUMAinMSA > 0.5) %>%
  transmute(MSA, STATEFIP, PUMA) %>%
  expand(nesting(MSA, STATEFIP, PUMA), year = 2012:2021)

MSA_PUMA10_12 <- PUMA10 %>%
  filter(PctPUMAinMSA_12 > 0.5) %>%
  transmute(MSA = MSA_12, STATEFIP, PUMA) %>%
  expand(nesting(MSA, STATEFIP, PUMA), year = 2012:2021)

FIPS_PUMA10 <- PUMA10 %>%
  filter(PctPUMAinFIPS > 0.5) %>%
  transmute(FIPS, STATEFIP, PUMA) %>%
  expand(nesting(FIPS, STATEFIP, PUMA), year = 2012:2021)

# 2022 to present
# Data from the Missouri Census Data Center: https://mcdc.missouri.edu/applications/geocorr2022.html

PUMA20 <- read_csv(paste0(path, "/puma_2020.csv"),
                   skip = 2,
                   col_names = c("state", "puma22", "county", "stab", "CountyName", "PUMA22name", "pop20", "afact"),
                   col_types = "cnccccnn")

PUMA20 %<>%
  transmute(
    STATEFIP = state,
    PUMA = puma22,
    FIPS = county,
    pop = pop20) %>%
  group_by(FIPS) %>%
  mutate(PctPUMAinFIPS = pop / sum(pop)) %>%
  select(STATEFIP, PUMA, FIPS, PctPUMAinFIPS) %>%
  ungroup()

PUMA20 %<>%
  left_join(MSA_FIPS, by = "FIPS") %>%
  group_by(MSA, STATEFIP, PUMA) %>%
  mutate(PctPUMAinMSA = sum(PctPUMAinFIPS)) %>%
  ungroup() %>%
  left_join(MSA_FIPS_12, by = "FIPS") %>%
  group_by(MSA_12, STATEFIP, PUMA) %>%
  mutate(PctPUMAinMSA_12 = sum(PctPUMAinFIPS)) %>%
  ungroup()

MSA_PUMA_20 <- PUMA20 %>%
  filter(PctPUMAinMSA > 0.5) %>%
  transmute(MSA, STATEFIP, PUMA) %>%
  expand(nesting(MSA, STATEFIP, PUMA), year = 2022:2024)

MSA2012_PUMA20 <- PUMA20 %>%
  filter(PctPUMAinMSA_12 > 0.5) %>%
  transmute(MSA = MSA_12, STATEFIP, PUMA) %>%
  expand(nesting(MSA, STATEFIP, PUMA), year = 2022:2024)

FIPS_PUMA20 <- PUMA20 %>%
  filter(PctPUMAinFIPS > 0.5) %>%
  transmute(FIPS, STATEFIP, PUMA) %>%
  expand(nesting(FIPS, STATEFIP, PUMA), year = 2022:2024)

# Combine files

MSA_PUMA     <- bind_rows(MSA_PUMA00, MSA_PUMA10, MSA_PUMA_20)
MSA2012_PUMA <- bind_rows(MSA_PUMA00_12, MSA_PUMA10_12, MSA2012_PUMA20)
FIPS_PUMA    <- bind_rows(FIPS_PUMA00, FIPS_PUMA10, FIPS_PUMA20)

MSA_PUMA %<>%
  pull_peers(add_info = F) %>%
  select(all_of(c("MSA", "year", "STATEFIP", "PUMA")))

MSA2012_PUMA %<>%
  pull_peers(add_info = F, geog = "MSA_2012") %>%
  select(all_of(c("MSA", "year", "STATEFIP", "PUMA")))

FIPS_PUMA %<>%
  pull_peers(add_info = F) %>%
  select(all_of(c("FIPS", "year", "STATEFIP", "PUMA")))

usethis::use_data(FIPS_PUMA, MSA_PUMA, MSA2012_PUMA, overwrite = TRUE)
