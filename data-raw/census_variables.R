library(stringr)
library(censusapi)
library(tidyr)
library(furrr)
library(future)
library(glptools)

glp_load_packages()

# Decennial Census 2000 population
df <- listCensusMetadata("dec/sf1", vintage = 2000, type = "variables")

df %<>%
  transmute(
    table = str_remove(group, "(?<=P)0*"), # Table ID appears before period in concept
    topic = concept, # Table topic appears after period in concept
    year = 2000,
    survey = "dec/sf1",
    variable = name,
    label)

census_api_vars <- assign_row_join(census_api_vars, df)

# Decennial Census 2010 population
df <- listCensusMetadata("dec/sf1", vintage = 2010, type = "variables")

df %<>%
  transmute(
    table = group, # Table ID appears before period in concept
    topic = concept, # Table topic appears after period in concept
    year = 2010,
    survey = "dec/sf1",
    variable = name,
    label)

census_api_vars <- assign_row_join(census_api_vars, df)

# Decennial Census 2020 population
df <- listCensusMetadata("dec/dhc", vintage = 2020, type = "variables")

df %<>%
  transmute(
    table = group,
    topic = concept,
    year = 2020,
    survey = "dec/dhc",
    variable = name,
    label)

census_api_vars <- assign_row_join(census_api_vars, df)

# Decennial Census 2000

df <- listCensusMetadata("dec/sf3", vintage = 2000, type = "variables")

df %<>%
  transmute(
    table = group, # Table ID appears before period in concept
    topic = concept, # Table topic appears after period in concept
    year = 2000,
    survey = "dec/sf3",
    variable = name,
    label)

census_api_vars <- assign_row_join(census_api_vars, df)

# ACS 1-year data

for (y in c(2005:2019, 2021:2023)) {

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

for (y in 2009:2023) {

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

word_list <- c(1:105, "Householder", "Under", "under", "year", "years", "and", "or", "to", "over", "")

select_age <- function(segment) {

  selection <- segment %>%
    str_split(" ") %>%
    purrr::map(~ all(. %in% word_list)) %>%
    as.logical()

  selection[segment == ""] <- FALSE

  segment[selection]
}

plan(multisession)

census_api_vars$age_text <- census_api_vars$label %>%
  str_split("!!|:|--") %>%
  furrr::future_map(select_age) %>%
  as.character() %>%
  replace(. == "character(0)", "") %>%
  str_trim()

census_api_vars %<>%
  mutate(
    age_group = case_when(

    # If "by age" or "age by" is not included in the title, age_group is NA
    str_detect(age_text, regex("by age|age by", ignore_case = TRUE), negate = FALSE) ~ NA_character_,

    # for ages of format "under x and x to y", label is 0_y
    str_detect(age_text, "(?i)under \\d.*\\d to \\d") ~
      paste0("0_",
             str_extract(age_text, "\\d*(?= years)")),

    # for ages of format "x to y", label is x_y
    str_detect(age_text, "\\d to \\d* years") ~
      paste0(str_extract(age_text, "\\d*(?= to \\d* years)"), "_",
             str_extract(age_text, "\\d*(?= years)")),

    # for ages of format "x years and over", label is x_plus
    str_detect(age_text, "\\d years (and|or) over") ~
      paste0(str_extract(age_text, "\\d*(?= years (and|or) over)"), "_", "plus"),

    # For poverty thresholds, descriptions of householders, veterans, no age label
    # formats: "under x.y" "own children
    str_detect(age_text, "^(?i)under \\d\\.|With.*children|No.*children|Responsibility|Served")  ~ NA_character_,

    # for ages of format "under x", label is "under_x"
    str_detect(age_text, "(?i)under \\d") ~ paste0("under_", str_extract(age_text, "(?<=(?i)under )\\d*")),

    # for ages of format "x and y", label is "x_y"
    str_detect(age_text, "\\d and \\d* years") ~
      paste0(str_extract(age_text, "\\d*(?= and )"), "_", str_extract(age_text, "\\d*(?= years)")),

    # for ages of format "x years", label is "x"
    str_detect(age_text, "\\d (?i)years") ~
      str_extract(age_text, "\\d*(?= years)"),

    # for ages of the format "1 year", label is "1"
    str_detect(age_text, "^1 year$") ~ "1"))

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
                          "total")))

# Separate age group into two columns
census_api_vars %<>%
  separate(col = age_group, into = c("age_low", "age_high"), convert = TRUE, remove = F) %>%
  # replace "under" with 0, "plus" with Inf.
  # Copy single-year age groups to both columns, and change the "high" column for "under x" to x-1
  mutate(
    age_low  = if_else(age_low == "under", 0, as.numeric(age_low)),
    age_high = if_else(age_high == "plus", Inf, as.numeric(age_high)),
    age_high = if_else(!is.na(age_low) & is.na(age_high), age_low, age_high),
    age_high = if_else(str_detect(age_group, "under"), age_high - 1, age_high)) %>%
  # fill columns for data not broken down by age
  mutate(
    age_group = if_else(is.na(age_group), "all", age_group),
    age_low   = if_else(is.na(age_low), 0, age_low),
    age_high  = if_else(is.na(age_high), Inf, age_high))

census_api_vars %<>%
  select(table, topic, year, survey, variable, label, race, sex, age_text, age_group, age_low, age_high)

# Updating sysdata
temp_env <- new.env()
load("R/sysdata.rda", envir = temp_env)

temp_env[["census_api_vars"]] <- census_api_vars

save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)



