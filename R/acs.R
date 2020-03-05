#' Process ACS microdata from IPUMS.
#'
#' Adds FIPS codes and the Tulsa MSA code, if appropriate.
#' Process race, sex, and education variables if present.
#'
#' @param df A dataframe of microdata from IPUMS.
#' @param gq Include group quarters residents? Defaults to \code{FALSE}.
#' @param pull_peers Subset the data to peers? Defaults to \code{TRUE}.
#' Subsets to peer MSAs if present. Otherwise, subsets to peers counties.
#' @export
process_acs <- function(df, gq = T, pull_peers = T, remove_vars = T){

  # Rename some columns
  suppressWarnings(df %<>% rename_at(df %cols_in% c("YEAR", "AGE", "SEX"), funs(str_to_lower)))

  # Remove group quarters residents if gq = FALSE
  if (!gq) df %<>% filter(GQ == 1 | GQ == 2)

  # Rename the MSA column and label the Tulsa MSA
  if ("MET2013" %in% names(df)) {

    df %<>% rename(MSA = MET2013)

    df$MSA[df$STATEFIP == 40 & df$year <= 2011 & df$PUMA %in% c(1000, 1100, 1200)] <- 46140
    df$MSA[df$STATEFIP == 40 & df$year >= 2012 & df$PUMA %in% c(1201, 1202, 1203, 1204, 1301)] <- 46140
  }

  # Add FIPS codes to data and change St. Louis FIPS codes to MERGED
  df %<>% left_join(FIPS_PUMA, by = c("STATEFIP", "PUMA", "year"))

  df$FIPS[df$FIPS == 29189] = "MERGED"
  df$FIPS[df$FIPS == 29510] = "MERGED"

  # df %<>% mutate(replace(FIPS, STATEFIP == 21 & PUMA %in% c(1901, 1902), "21067")) # Adds in Lexington

  # Subset data to peers at the MSA or county level
  if ("MSA" %in% names(df) & pull_peers) {
    df %<>% pull_peers(add_info = FALSE)
  } else if (pull_peers) {
    df %<>% pull_peers(add_info = FALSE)
  }

  # Recode race
  if ("RACE" %in% names(df)) {
    df$race <- "other"
    df$race[df$RACE == 1 & df$HISPAN == 0] <- "white"
    df$race[df$RACE == 2 & df$HISPAN == 0] <- "black"
    df$race[df$HISPAN %in% 1:4] <- "hispanic"

    df %<>% select(-RACE, -RACED, -HISPAN, -HISPAND)
  }

  # Recode sex
  if ("sex" %in% names(df)) {
    df$sex <- if_else(df$sex == 1, "male", "female")
  }

  # Recode education
  if ("EDUCD" %in% names(df)) {
    df %<>%
      mutate(
        educ = "no_hs",
        educ = replace(educ, EDUCD %in% c(62, 63, 64), "hs"),
        educ = replace(educ, EDUCD %in% c(65, 71), "some_col"),
        educ = replace(educ, EDUCD == 81, "assoc"),
        educ = replace(educ, EDUCD == 101, "bach"),
        educ = replace(educ, EDUCD %in% c(114, 115, 116), "grad"),
        educ = replace(educ, EDUCD == 1, NA)) %>%
      select(-EDUC, -EDUCD)
  }

  # Remove some variables that are no longer needed

  if (remove_vars) df %<>% select(df %cols_not_in% c("GQ", "STATEFIP", "PUMA", "DATANUM", "CBSERIAL" ))

  df
}

#' Process CPS microdata from IPUMS.
#'
#' Process race, sex, and education variables if present.
#'
#' @param df A dataframe of microdata from IPUMS.
#' @param pull_peers Subset the data to peers? Defaults to \code{TRUE}.
#' Subsets to peer MSAs if present. Otherwise, subsets to peers counties.
#' @export
process_cps <- function(df, pull_peers = T){

  # Rename some columns
  suppressWarnings(
    df %<>%
      rename_at(df %cols_in% c("YEAR", "AGE", "SEX"),
                funs(str_to_lower))
  )

  # Rename the MSA column and label the Tulsa MSA
  if ("METFIPS" %in% names(df)) df %<>% rename(MSA = METFIPS)
  if ("COUNTY" %in% names(df)) df %<>% rename(FIPS = COUNTY)

  # Rename St. Louis
  df$FIPS[df$FIPS == 29189] = 'MERGED'
  df$FIPS[df$FIPS == 29510] = 'MERGED'

  # Subset data to peers at the MSA or county level
  if ("MSA" %in% names(df) & pull_peers) {
    df %<>% pull_peers(add_info = FALSE)
  } else if (pull_peers) {
    df %<>% pull_peers(add_info = FALSE)
  }

  # Recode race
  if ("RACE" %in% names(df)) {
    df$race <- "other"
    df$race[df$RACE == 100 & df$HISPAN == 0] <- "white"
    df$race[df$RACE == 200 & df$HISPAN == 0] <- "black"
    df$race[df$HISPAN != 0] <- "hispanic"

    df %<>% select(-RACE, -HISPAN)
  }

  # Recode sex
  if ("sex" %in% names(df)) {
    df$sex <- if_else(df$sex == 1, "male", "female")
  }

  # Recode education
  if ("EDUC" %in% names(df)) {
    df %<>%
      mutate(
        educ = "no_hs",
        educ = replace(educ, EDUC == 73, "hs"),
        educ = replace(educ, EDUC == 81, "some_col"),
        educ = replace(educ, EDUC %in% c(91, 92), "assoc"),
        educ = replace(educ, EDUC == 111, "bach"),
        educ = replace(educ, EDUC %in% 123:125, "grad"),
        educ = replace(educ, EDUC == 1, NA)) %>%
      select(-EDUC)
  }

  df
}


#' Survey microdata by race and sex
#'
#' This function uses the survey package to break down estimates by demographic group.
#'
#' @param survey A survey object containing FIPS, year, and optional race and sex columns.
#' @param var A column name to perform svymean on.
#' @param race Break down by race? Defaults to \code{TRUE}.
#' @param sex Break down by sex? Defaults to \code{TRUE}.
#' @param cross Break down by race and sex? Defaults to \code{TRUE}.
#' @export
svy_race_sex <- function(df, var, weight_var = "PERWT", geog = "FIPS"){

  var <- as.character(substitute(var))

  #Total
  results <- df %>%
    group_by_at(c(geog, "year")) %>%
    summarise(!!var := weighted.mean(.data[[var]], .data[[weight_var]], na.rm = TRUE)) %>%
    mutate(
      sex = "total",
      race = "total")

  #By sex
  results_sex <- df %>%
    group_by_at(c(geog, "year", "sex")) %>%
    summarise(!!var := weighted.mean(.data[[var]], .data[[weight_var]], na.rm = TRUE)) %>%
    mutate(race = "total")

  results %<>% bind_rows(results_sex)

  #By race
  results_race <- df %>%
    group_by_at(c(geog, "year", "race")) %>%
    summarise(!!var := weighted.mean(.data[[var]], .data[[weight_var]], na.rm = TRUE)) %>%
    mutate(sex = "total")

  results %<>% bind_rows(results_race)

  #By race and sex
  results_race_sex <-  df %>%
    group_by_at(c(geog, "year", "race", "sex")) %>%
    summarise(!!var := weighted.mean(.data[[var]], .data[[weight_var]], na.rm = TRUE))

  results %<>% bind_rows(results_race_sex)

  results %<>%
    ungroup() %>%
    filter(
      !is.na(.data[[geog]]),
      race != "other")

  results
}

#' Survey microdata by race and sex on a categorical variable
#'
#' This function uses the survey package to break
#'   down estimates by demographic group.
#'
#' @param survey A survey object containing FIPS, year, and optional race and sex columns.
#' @param var A column name to perform svymean on.
#' @export
svy_race_sex_cat <- function(df, var, weight_var = "PERWT", geog = "FIPS"){

  #browser()

  var <- as.character(substitute(var))

  #Total
  results <- df %>%
    filter(
      !is.na(.data[[var]]),
      !is.na(.data[[geog]])) %>%
    group_by_at(vars(geog, "year", var, race, sex)) %>%
    summarise(freq = sum(.data[[weight_var]])) %>%
    ungroup()

  total_sex_race <- results %>%
    group_by_at(vars(!!geog, "year", race, sex)) %>%
    mutate(pct = freq / sum(freq))

  total_sex <- results %>%
    group_by_at(vars(!!geog, year, race, !!var)) %>%
    summarise(freq = sum(freq)) %>%
    ungroup() %>%
    group_by_at(vars(!!geog, year, race)) %>%
    mutate(
      pct = freq / sum(freq),
      sex = "total")

  total_race <- results %>%
    group_by_at(vars(!!geog, year, sex, !!var)) %>%
    summarise(freq = sum(freq)) %>%
    ungroup() %>%
    group_by_at(vars(!!geog, year, sex)) %>%
    mutate(
      pct = freq / sum(freq),
      race = "total")

  total <- results %>%
    group_by_at(vars(!!geog, year, !!var)) %>%
    summarise(freq = sum(freq)) %>%
    ungroup() %>%
    group_by_at(vars(!!geog, year)) %>%
    mutate(
      pct = freq / sum(freq),
      race = "total",
      sex = "total")

  output <- bind_rows(total, total_sex, total_race, total_sex_race)

  output %<>%
    ungroup() %>%
    select(-freq) %>%
    mutate(pct = pct * 100) %>%
    filter(race != "other") %>%
    spread(key = !!var, value = pct)

  output %<>%
    mutate_at(output %cols_not_in% c("FIPS", "year", "race", "sex"), ~ replace_na(., 0)) %>%
    organize()

  output
}
