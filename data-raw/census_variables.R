library(stringr)
library(censusapi)
library(glptools)

# Decennial Census 2000

df <- listCensusMetadata("sf3", vintage = 2000, type = "variables")

df %<>%
  transmute(
    table = str_extract(concept, "^.*?(?=\\.)"), # Table ID appears before period in concept
    topic = str_extract(concept, "(?<=\\. ).*"), # Table topic appears after period in concept
    year = 2000,
    survey = "sf3",
    variable = name,
    label)

census_api_vars <- assign_row_join(census_api_vars, df)

# ACS 1-year data

for (y in 2005:2018) {

  print(y)

  df <- listCensusMetadata("acs/acs1", vintage = y, type = "variables")

  df %<>%
    transmute(
      table = group,
      topic = concept,
      year = y,
      survey = "acs/acs1",
      variable = name,
      label)

  census_api_vars <- assign_row_join(census_api_vars, df)
}

# ACS 5-year data

for (y in 2009:2018) {

  print(y)

  acs_label <- if (y == 2009) "acs5" else "acs/acs5"

  df <- listCensusMetadata(acs_label, vintage = y, type = "variables")

  if (y == 2009) {
    df %<>% mutate(group = str_extract(concept, "^.*?(?=\\.)"),)
  }

  df %<>%
    transmute(
      table = group,
      topic = concept,
      year = y,
      survey = acs_label,
      variable = name,
      label)

  census_api_vars <- assign_row_join(census_api_vars, df)
}

# Add race labels

race_2000 <- c(A = "white_maybe_hispanic", B = "black", C = "AIAN", D = "asian", E = "hawaiian_PI",
               `F` = "other", G = "two_plus", H = "hispanic", I = "white")

race_acs <- c(A = "white_maybe_hispanic", B = "black", C = "AIAN", D = "asian", E = "hawaiian_PI",
              `F` = "other", G = "two_plus", H = "white", I = "hispanic")

census_api_vars %<>%
  filter(!is.na(table) & table != "N/A") %>%
  mutate(race_code = str_extract(table, "(?<=\\d)[A-I]")) %>%
  mutate(
    race = if_else(year == 2000,
                   recode(race_code, !!!race_2000, .missing = "total"),
                   recode(race_code, !!!race_acs, .missing = "total")),
    sex = if_else(str_detect(label, "Female"), "female",
                  if_else(str_detect(label, "Male"), "male",
                          "total"))) %>%
  select(-race_code)

usethis::use_data(census_api_vars, overwrite = TRUE)
