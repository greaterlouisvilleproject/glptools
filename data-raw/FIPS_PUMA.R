library(readr)
library(dplyr)
library(stringr)
library(magrittr)
source("R/general.R")

path <- "data-raw/FIPS_PUMA/"

# 2000 to 2011

puma_00 <- read_fwf(path %p% "puma2county.txt",
                    fwf_positions(start = c(1, 4, 9,  13, 21, 25, 34, 43, 50, 58, 66),
                                  end   = c(2, 8, 12, 17, 22, 31, 40, 49, 57, 65, NA),
                                  col_names = c("STATEFIP", "PUMA", "countyFIPS",	"MSA", "PMSA", "PUMApop",
                                                "FIPSPop", "PUMApopinFIPS", "PctPUMAinFIPS", "PctFIPSinPUMA", "Name")),
                    col_types = "cncccnnnnnc")

# Create FIPS codes from State and County FIPS, then subset.
puma_00 %<>%
  mutate(
    STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = "0"),
    countyFIPS = str_pad(countyFIPS, width = 3, side = "left", pad = "0"),
    FIPS = as.numeric(paste0(STATEFIP, countyFIPS)),
    STATEFIP = as.numeric(STATEFIP)) %>%
  pull_peers_FIPS(add_info = FALSE) %>%
  filter(PctPUMAinFIPS > 0.5) %>%
  select(STATEFIP, PUMA, FIPS)

puma_00 <- data.frame(
  puma_00,
  year = rep(c(2000, 2005:2011), each = nrow(puma_00)))


# 2012 and on

puma_2010_files <- list.files(path %p% "PUMA_FIPS_10")

for(f in puma_2010_files){
  df <- read_fwf(paste0(path, "PUMA_FIPS_10", "/", f),
                 fwf_cols(SL = 3, STATEFIP = 2, STATENS = 8, PUMA = 5,
                          COUNTYFIPS = 3, COUNTYns = 8, COUSUBFP = 5,
                          COUSUBNS = 8, PLACEFP = 5, PLACENS = 8, TRACTCE = 6,
                          pop = 9, HU10 = 9, NAME = 100),
                 col_types = "cncnccnncccnnc")

  # Create FIPS codes from State and County FIPS, then subset.
  df %<>%
    filter(SL == "796") %>%
    mutate(
      FIPS = paste0(STATEFIP, COUNTYFIPS),
      PUMA = PUMA,
      STATEFIP = STATEFIP) %>%
    group_by(PUMA) %>%
    mutate(PctPUMAinFIPS = pop / sum(pop)) %>%
    filter(PctPUMAinFIPS > 0.5) %>%
    select(STATEFIP, PUMA, FIPS) %>%
    ungroup()

  if(!exists("puma_10")){
    puma_10 <- df
  } else {
    puma_10 %<>% bind_rows(df)
  }
}

puma_10 %<>% pull_peers_FIPS(add_info = FALSE)

puma_10 <- data.frame(
  puma_10,
  year = rep(2012:2017, each = nrow(puma_10))) %>%
  select(STATEFIP, PUMA, year, FIPS)

FIPS_PUMA <- bind_rows(puma_00, puma_10)

FIPS_PUMA %<>% select(FIPS, year, STATEFIP, PUMA)

usethis::use_data(FIPS_PUMA, overwrite = TRUE)


