#' Process ACS microdata from IPUMS.
#'
#' Adds FIPS codes and the Tulsa MSA code, if appropriate. Process race, sex, and education variables if present.
#'
#' @param df A dataframe of microdata from IPUMS.
#' @param gq Include group quarters residents? Defaults to \code{FALSE}.
#' @param pull_peers Subset the data to peers? Defaults to \code{TRUE}.
#' Subsets to peer MSAs if present. Otherwise, subsets to peers counties.
#' @export
clean_acs_micro <- function(df, gq = T, pull_peers = T, remove_vars = T){

  # Rename some columns
  suppressWarnings(df %<>% rename_at(df %cols_in% c("YEAR", "AGE", "SEX"), funs(str_to_lower)))

  # Remove group quarters residents if gq = FALSE
  if (!gq) df %<>% filter(GQ == 1 | GQ == 2)

  # Add MSA Column
  MSA_PUMA %<>% mutate(STATEFIP = as.numeric(STATEFIP))

  df %<>% left_join(MSA_PUMA, by = c("STATEFIP", "PUMA", "year"))

  # Add FIPS codes to data and change St. Louis FIPS codes to MERGED
  FIPS_PUMA %<>% mutate(STATEFIP = as.numeric(STATEFIP))
  df %<>% left_join(FIPS_PUMA, by = c("STATEFIP", "PUMA", "year"))

  df %<>% stl_merge(just_replace_FIPS = TRUE)

  # df %<>% mutate(replace(FIPS, STATEFIP == 21 & PUMA %in% c(1901, 1902), "21067")) # Adds in Lexington

  # Subset data to peers at the MSA or county level
  if (pull_peers) df %<>% pull_peers(add_info = FALSE)

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
clean_cps_micro <- function(df, pull_peers = T){

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
#' @param survey A survey object containing FIPS, year, and optional race and sex columns.
#' @param var A column name to perform svymean on.
#' @param type Either mean or categorical
#' @param weight_var weight variable
#' @param geog geographic column
#' @param other_grouping_vars other variables to group by
#' @export
survey_by_demog <- function(df, var,
                            weight_var = "PERWT",
                            geog = "FIPS",
                            method = "default",
                            type = "categorical",
                            other_grouping_vars = c(),
                            breakdowns = c("total", "sex", "race", "sex_by_race")) {

  var <- as.character(substitute(var))

  # Remove missing values of variable or geography
  df %<>%
    filter(
      !is.na(.data[[var]]),
      !is.na(.data[[geog]]))

  # Use the bootstrap or replicate weights function?
  if (method != "bootstrap" &
      ("REPWT1" %in% names(df) | "REPWTP1" %in% names(df))) {
    svy_fxn <- glptools:::svy_repwts
  } else {
    svy_fxn <- glptools:::svy_bootstrap
  }

  # Calculate data at different demographic levels, then join by rows
  if ("sex_by_race" %in% breakdowns) {
    total_by_sex_race <- svy_fxn(df, var, weight_var, type,
                                 c(geog, "year", "sex", "race", other_grouping_vars))
    output <- assign_row_join(output, total_by_sex_race)
  }
  if ("sex" %in% breakdowns) {
    total_by_sex      <- svy_fxn(df, var, weight_var, type,
                                 c(geog, "year", "sex", other_grouping_vars))   %>% mutate(race = "total")
    output <- assign_row_join(output, total_by_sex)
  }
  if ("race" %in% breakdowns) {
    total_by_race     <- svy_fxn(df, var, weight_var, type,
                                 c(geog, "year", "race", other_grouping_vars)) %>% mutate(sex = "total")
    output <- assign_row_join(output, total_by_race)
  }
  if ("total" %in% breakdowns) {
    total             <- svy_fxn(df, var, weight_var, type,
                                 c(geog, "year", other_grouping_vars)) %>% mutate(sex = "total", race = "total")
    output <- assign_row_join(output, total)
  }

  # If categorical variable, add it to grouping variables.
  all_grouping_vars <- c(geog, "year", "sex", "race", other_grouping_vars)
  if (type == "categorical") all_grouping_vars <- c(all_grouping_vars, var)

  # Complete data to add in explicit missing combinations
  # Replace missing estimates and percentages with 0
  output %<>%
    complete_vector_arg(all_grouping_vars) %>%
    mutate(across(any_of(c("estimate", "percent")), ~ replace(., is.na(.), 0)))

  # Fill in any missing population data
  if (type == "categorical") {
    output %<>%
      group_by_at(c(geog, "year", "sex", "race", other_grouping_vars)) %>%
      fill(population, .direction = "downup") %>%
      ungroup()
  }

  output %<>%
    pivot_longer(cols = any_of(c("amount.lower", "amount.estimate", "amount.upper",
                                 "percent.lower", "percent.estimate", "percent.upper",
                                 "estimate", "MOE", "CI", "percent",
                                 "population")),
                 names_to = "var_type")

  if (type == "categorical" & typeof(df[[var]]) == "character") {
    output %<>%
      pivot_wider(names_from = var, values_from = value)
  } else if (type == "categorical" & typeof(df[[var]]) == "logical"){
    output %<>%
      filter(across(all_of(var), ~ .))

    output %<>%
      select(-var) %>%
      rename(!!var := value)
  } else if (type == "mean"){
    output %<>% rename(!!var := value)
  }

  output
}

#' Survey microdata by a particular demographic grouping
#'
#' @param df A data frame of survey data
#' @param var The variable of interest
#' @param weight_var A column of weights
#' @param grouping_vars Columns to group by
svy_bootstrap <- function(df, var, weight_var, type, grouping_vars) {

  # Function to pass to bootstrap function
  # Calculates number, population, and percent
  calc_output <- function(d, var, weight_var, type, var_level, ...) {

    d <- analysis(d)

    population <- sum(d[[weight_var]])

    if (type == "categorical") {
      amount <- sum(d[[weight_var]][d[[var]] == var_level], na.rm=TRUE)

      output <- data.frame(
        term = c("amount", "population", "percent"),
        estimate = c(amount, population, amount / population * 100))

    } else if (type == "mean") {
      amount <- weighted.mean(d[[var]], d[[weight_var]], na.rm = TRUE)

      output <- data.frame(
        term = c("amount", "population"),
        estimate = c(amount, population))
    }

    output
  }

  var_levels <- unique(df[[var]])

  df %<>%
    filter(across(all_of(grouping_vars), ~!is.na(.))) %>%
    group_by(across(all_of(grouping_vars))) %>%
    nest()

  if (type == "categorical") {

    # Remove NA groups and create a boot object for each group
    for (l in var_levels) {

      results <- df %>%
        # Create column of bootstrap samples for each row
        # Samples is a list-column of data frames, where each data frame
        #   contains two columns consisting of 1000 splits and bootstrap IDs.
        mutate(samples = map(data, ~bootstraps(., 1000, apparent = TRUE))) %>%

        # Analyse each bootstrap replicate by applying calc_output to each replicate.
        # Mapping into samples accesses the tibble of bootstrap samples for each demographic
        # Mutate adds a column to the tibble of bootstrap samples
        # The second map creates the model for each bootstrap sample
        mutate(models = map(samples, ~mutate(., results = map(splits, ~calc_output(., var, weight_var, type, var_level = l))))) %>%

        # To create BcA estimates, access the results column within each bootstrap sample
        mutate(intervals = map(models, ~int_bca(., results, 0.1, calc_output,
                                                var, weight_var, type, var_level = l))) %>%

        # Unnest the data frame
        select(-data, -samples, -models) %>%
        unnest(cols = c(intervals))

      results %<>%
        mutate(!!var := l)

      output <- assign_row_join(output, results)

    }
  } else if (type == "mean") {

    output <- df %>%
      mutate(samples = map(data, ~bootstraps(., 1000, apparent = TRUE))) %>%
      mutate(models = map(samples, ~mutate(., results = map(splits, ~calc_output(., var, weight_var, type))))) %>%
      mutate(intervals = map(models, ~int_bca(., results, 0.1, calc_output, var, weight_var, type))) %>%
      select(-data, -samples, -models) %>%
      unnest(cols = c(intervals))
  }

  # Extract relevant data
  # Reshape data to wide format
  output %<>%
    ungroup() %>%
    select(-.alpha, -.method) %>%
    pivot_wider(names_from = term,
                values_from = c(.lower, .estimate, .upper),
                names_glue = "{term}{.value}") %>%
    select(-population.lower, -population.upper) %>%
    rename(population = population.estimate)

  output
}

#' Survey using replicate weights
#'
#' @param df A data frame of survey data
#' @param var The variable of interest
#' @param weight_var A column of weights
#' @param grouping_vars Columns to group by
svy_repwts <- function(df, var, weight_var, type, grouping_vars) {

  # Function to calculate MOE for specific combinations of variables
  add_MOE <- function(df, estimate, var, type, weight_var) {

    # If census microdata with REPWTS, use REPWT columns. Otherwise, bootstrap.
    repwts <- if_else(weight_var == "PERWT", "REPWTP", "REPWT")

    if (type == "categorical") {
      replicates <- df %>%
        select(all_of(paste0(repwts, 1:80))) %>%
        map_dbl(sum)

      sqrt(4/80 * sum((replicates - estimate)^2)) * 1.645
    } else if (type == "mean") {
      replicates <- df %>%
        select(all_of(paste0(repwts, 1:80))) %>%
        map_dbl( ~ weighted.mean(df[[var]], .x, na.rm = TRUE))

      sqrt(4/80 * sum((replicates - estimate)^2)) * 1.645
    }
  }

  # Filter out rows with missing groups and group by
  results_estimate <- df %>%
    filter(across(all_of(grouping_vars), ~!is.na(.)))

  # Estimate
  if (type == "categorical") {
    results_estimate %<>%
      group_by(across(all_of(c(grouping_vars, var)))) %>%
      summarise(estimate = sum(.data[[weight_var]]), .groups = "drop")

    results_MOE <- df %>%
      group_by(across(all_of(c(grouping_vars, var)))) %>%
      nest() %>%
      left_join(results_estimate, by = c(grouping_vars, var))

  } else if (type == "mean") {
    results_estimate %<>%
      group_by(across(all_of(grouping_vars))) %>%
      summarise(estimate = weighted.mean(.data[[var]], .data[[weight_var]], na.rm = TRUE), .groups = "drop")

    results_MOE <- df %>%
      group_by(across(all_of(grouping_vars))) %>%
      nest() %>%
      left_join(results_estimate, by = grouping_vars)
  }

  # Add MOE
  results_MOE %<>%
    mutate(MOE = map_dbl(data, ~add_MOE(., estimate, var, type, weight_var))) %>%
    select(-data) %>%
    ungroup()

  if (type == "categorical") {
    results_final <- results_MOE %>%
      group_by(across(all_of(grouping_vars))) %>%
      mutate(
        population = sum(estimate),
        percent    = estimate / population * 100,
        CI         = MOE / population * 100) %>%
      ungroup()
  }

  results_final
}

