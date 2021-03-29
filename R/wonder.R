#' Clean CDC Wonder data
#'
#' Processes a data frame of CDC Wonder mortality or natality data.
#' Renames and selects colums of interest, recodes age groups,
#' filters out rows with totals, recodes race variables, and merges St. Louis counties
#'
#' @param df A data frame.
#' @param method "sum" or "weight" to merge St. Louis.
#' @export
clean_wonder <- function(df, method = "sum"){

  #new names for variables
  names_recode <- c(
    `County Code`                  = "FIPS",
    Year                           = "year",
    `Single-Year Ages Code`        = "age",
    `Ten-Year Age Groups Code`     = "age_10_codes",
    Deaths                         = "deaths",
    Population                     = "population",
    `Birth Weight 12 Code`         = "weight",
    Births                         = "births",
    Race                           = "race",
    `Bridged Race`                 = "race",
    `Mother's Bridged Race`        = "race",
    Gender                         = "sex",
    `Age Adjusted Rate`            = "rate",
    `Multiple Cause of death Code` = "cod")


  #list of variables to keep (same as above list)
  keep_names <- c("FIPS", "year", "age", "age_10",
                  "deaths", "population", "weight", "births",
                  "race", "sex", "rate", "cod")

  #list of variables to change to numeric
  numeric_names <- c("year", "age", "deaths", "population",
                     "weight", "births", "rate")

  df <- df %>%
    plyr::rename(
      replace = names_recode,
      warn_missing = FALSE)

  if("age_10_codes" %in% names(df)){
    age_10_df <- data.frame(
      age_10_codes = c("1", "1-4", "5-14", "15-24", "25-34", "35-44",
                       "45-54", "55-64", "65-74", "75-84", "85+", "NS"),
      age_10 = c(0:10, NA))

    df %<>%
      left_join(age_10_df, by = "age_10_codes") %>%
      select(-age_10_codes)
  }

  #filter rows containing totals (which do not contain specific years)
  df <- df %>%
    filter_at(df %cols_in% c("FIPS", "year", "age", "age_10", "race", "sex"), all_vars(!is.na(.)))

  #convert variables to numeric and subset data frame to variables of interest
  df <- df %>%
    mutate_at(df %cols_in% numeric_names, as.numeric) %>%
    select_at(df %cols_in% keep_names)

  if("race" %in% names(df)){
    df %<>%
      mutate(
        race = replace(race, race == "Black or African American", "black"),
        race = replace(race, race == "White", "white"))
  }
  if("sex" %in% names(df)){
    df %<>% mutate(sex = str_to_lower(sex))
  }

  df %<>%
    pivot_longer(
      df %cols_in% c("births", "deaths", "rate", "population"),
      names_to = "var_type",
      values_to = df %cols_in% c("births", "deaths", "rate")) %>%
    mutate(var_type = case_when(
      var_type %in% c("births", "deaths") ~ "estimate",
      var_type %in% c("rate") ~ "rate",
      var_type %in% c("population") ~ "population"))

  #Merge Saint Louis Counties
  if ("FIPS" %in% names(df)) {
    df %<>% stl_merge(df %cols_in% c("births", "deaths", "rate"),
                      method = method,
                      other_grouping_vars = df %cols_in% c("age", "age_10")) %>%
      filter(var_type != "percent")
  }

  if ("race" %not_in% names(df)) df$race <- "total"
  if ("sex" %not_in% names(df)) df$sex <- "total"

  df %<>% organize()

  df

}

#' Calculate an age-adjusted rate
#'
#' @param df A data frame.
#' @param var The variable to weight.
#' @param age_var Variable of age groups. Either "age" or "age_10" from \code{clean_wonder}.
#' @param pop_var The popylatuin variable.
#' @export
age_adj_rate <- function(df, var, age_var = "age", pop_var = "population"){

  df$var <- df[[var]]
  df$age_var <- df[[age_var]]
  df$pop_var <- df[[pop_var]]

  grouping_vars <- c("FIPS", "year", "race", "sex")

  # Label age groups
  if(age_var == "age"){
    age_groups <-
      data.frame(
        age_var = 0:100,
        age_group = c(
          0,
          rep(1, 4),
          rep(2, 10),
          rep(3, 10),
          rep(4, 10),
          rep(5, 10),
          rep(6, 10),
          rep(7, 10),
          rep(8, 10),
          rep(9, 10),
          rep(10, 16)))

    df <- df %>%
      left_join(age_groups, by = c("age_var"))
  } else if(age_var == "age_10"){
    df$age_group <- df$age_var
  }

  # Summarise variable and population variable by age groups
  df <- df %>%
    group_by_at(df %cols_in% c("age_group", grouping_vars)) %>%
    summarise(
      var = sum(var, na.rm = TRUE),
      pop_var = sum(pop_var, na.rm = TRUE)) %>%
    ungroup()

  # Standard population from the CDC: https://wonder.cdc.gov/wonder/help/ucd.html#2000%20Standard%20Population
  std_pop <-
    data.frame(
      age_group = 0:10,
      weight = c(
        0.013818,
        0.055317,
        0.145565,
        0.138646,
        0.135573,
        0.162613,
        0.134834,
        0.087247,
        0.066037,
        0.044842,
        0.015508))

  # Subset standardized population to only ages present in the sample,
  #   standardize to total 1, and join to data frame
  std_pop <- std_pop %>%
    filter(age_group %in% df$age_group) %>%
    mutate(weight = weight / sum(weight))

  df <- df %>%
    left_join(std_pop, by = "age_group") %>%
    group_by_at(df %cols_in% grouping_vars) %>%
    summarise(rate = sum(var / pop_var * weight) * 100000) %>%
    ungroup()

  df[[var]] <- df$rate

  df <- df %>% select(-rate)

  df
}

# DEPRECATED

#' Merge St. Louis
#'
#' @param df A data frame.
#' @param method "sum" or "weight" to merge St. .
#' @param pop_var The population variable.
#' @export
stl_adj_wonder <- function(df, method = "sum", pop_var = "population"){

  #list of variables to group by for summarizing St. Louis data
  group_vars <- c("FIPS", "year", "age", "age_10", "weight", "race", "sex")

  #merge St. Louis city and county
  df <- df %>%
    mutate(FIPS = replace(FIPS, FIPS == "29189" | FIPS == "29510", "MERGED"))

  #if any variables that should be weighted are present, weight the data frame
  if(method == "sum"){
    df <- df %>%
      group_by_if(names(df) %in% group_vars) %>%
      summarise_all(sum) %>%
      ungroup()
  }
  #otherwise, sum variables for the two STL counties
  else if (method == "weight"){
    df <- df %>%
      group_by_if(names(df) %in% group_vars) %>%
      summarise_all(rate = weighted.mean(rate, pop_var)) %>%
      ungroup()
  }
  else(df <- NA)

  df
}
