#' Load GLP-related packages
#'
#' @param graphs Will graphs or maps be made?
#' @export
glp_load_packages <- function(graphs = F) {
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(magrittr)
  library(purrr)
  library(rsample)

  if (graphs) {
    library(showtext)
    library(scales)
    library(ggrepel)
    library(leaflet)
  }
}

#' Return the geography of a GLP data frame.
#'
#' Returns "county", "MSA", "kentucky_ed"
#'
#' @param df A data frame.
#' @export
df_type <- function(df){
  cols <- names(df)

  case_when(
    "PUMA" %in% cols       ~ "PUMA",
    "zip" %in% cols        ~ "zip",
    "FIPS" %in% cols       ~ "FIPS",
    "MSA"  %in% cols       ~ "MSA",
    "frl_status" %in% cols ~ "ky",
    all(cols %in% c("year", "variable", "category", "value"))         ~ "graph",
    all(cols %in% c("year", "city", "variable", "category", "value")) ~ "graph_max_min",
    "block" %in% cols                                           ~ "block",
    "block_group" %in% cols ~ "block_group",
    "tract" %in% cols                                           ~ "tract",
    "market" %in% cols                                          ~ "market",
    "county" %in% cols                                          ~ "county",
    "neighborhood" %in% cols & "Phoenix Hill-Smoketown-Shelby Park" %in% df[["neighborhood"]] ~ "nh",
    "neighborhood" %in% cols                                    ~ "muw",
    TRUE ~ NA_character_)
}


#' Joins data frames by common GLP ID variables
#'
#' @param ... Data frames.
#' @export
bind_df <- function(..., by = NULL){
  data_frames <- list(...)

  grouping_vars <- c("FIPS", "MSA", "zip", "tract", "neighborhood", "disctrict", "year",
                     "race", "sex", "frl_status", "demographic", "var_type", "variable")

  if (is.null(by)) {
    grouping_vars <- grouping_vars[grouping_vars %in% names(data_frames[[1]])]
  }
  else {
    grouping_vars <- by
  }

  output <- purrr::reduce(data_frames, full_join, by = grouping_vars)

  output %<>% organize()

  output
}

#' Gathers data from wide format into long format based on sex.
#'
#' Variables should end in ".male", ".female", or "" (blank for totals).
#' Variable names should not contain other periods.
#'
#' @param df A data frame
#' @export
reshape_sex <- function(df) {

  geog <- df_type(df)

  df %<>%

    #gather columns
    gather(-!!geog, -year, key = "variable", value = "value") %>%

    #divide columns at "."
    separate(variable, c("variable", "sex"), "\\.", extra = "drop", fill = "right") %>%

    #replace male+female columns with "total"
    mutate(sex = replace_na(sex, "total")) %>%

    #reshape data to side format
    spread(key = variable, value = value)

  df
}

#' Aggregates demographic data
#'
#' @param df A data frame
#' @param ... Variables to total
#' @export
total_demographics <- function(df, ..., total_sex = T, total_race = F, include_na = F, other_grouping_vars = "", moe = T) {

  variables <- dplyr:::tbl_at_vars(df, vars(...))
  grouping_vars <- df %cols_in% c("MSA", "FIPS", "zip", "tract", "neighborhood", "block_group",
                                  "year", "race", "sex", other_grouping_vars)

  total_sex  <- total_sex & any(df$sex != "total")
  total_race <- total_race & any(df$race != "total")

  # Summarize data frame by race and sex.
  if (total_sex) {

    if (include_na) df_tot_sex <- filter(df, sex != "total" | is.na(sex))
    else df_tot_sex <- df %>% filter(sex != "total")

    df_tot_sex %<>%
      pivot_vartype_wider(variables) %>%
      group_by(across(c("variable", setdiff(grouping_vars, "sex")))) %>%
      sum_by_var_type() %>%
      mutate(sex = "total") %>%
      pivot_vartype_longer()
  }

  if (total_race) {

    if (include_na) {
      df_tot_race <- filter(df, race != "total" | is.na(race))
    } else{
      df_tot_race <- filter(df, race != "total")
    }

    df_tot_race %<>%
      pivot_vartype_wider(variables) %>%
      group_by(across(c("variable", setdiff(grouping_vars, "race")))) %>%
      sum_by_var_type() %>%
      mutate(race = "total") %>%
      pivot_vartype_longer()
  }

  if (total_sex & total_race) {

    if (include_na) df_tot <- filter(df, race != "total" | is.na(race),
                                     sex != "total" | is.na(sex))
    else df_tot <- filter(df, sex != "total", race != "total")

    df_tot %<>%
      pivot_vartype_wider(variables) %>%
      group_by(across(c("variable", setdiff(grouping_vars, c("race", "sex"))))) %>%
      sum_by_var_type() %>%
      mutate(sex = "total", race = "total") %>%
      pivot_vartype_longer()
  }

  # Fill in any total values that are not present in the data
  # or are NA with totals
  # Go by variable in case NA values differ across variables
  for (v in variables) {

    # Keep original data frame values where v is not NA
    df_not_na <- df %>%
      filter(!is.na(.data[[v]])) %>%
      select(all_of(c(grouping_vars, "var_type", v)))

    # Filter total data frames to combinations not included in df_not_na and join to df_not_na
    if (total_sex) {
      this_df_tot_sex  <- df_tot_sex  %>%
        filter(!is.na(.data[[v]])) %>%
        anti_join(df_not_na, by = grouping_vars) %>%
        select(all_of(c(grouping_vars, "var_type", v)))

      df_not_na %<>% bind_rows(this_df_tot_sex)
    }

    if (total_race) {
      this_df_tot_race  <- df_tot_race  %>%
        filter(!is.na(.data[[v]])) %>%
        anti_join(df_not_na, by = grouping_vars) %>%
        select(all_of(c(grouping_vars, "var_type", v)))

      df_not_na %<>% bind_rows(this_df_tot_race)
    }

    if (total_sex & total_race) {
      this_df_tot  <- df_tot %>%
        filter(!is.na(.data[[v]])) %>%
        anti_join(df_not_na, by = grouping_vars) %>%
        select(all_of(c(grouping_vars, "var_type", v)))

      df_not_na %<>% bind_rows(this_df_tot)
    }

    output <- assign_col_join(output, df_not_na, by = c(grouping_vars, "var_type"))
  }

  output %<>%
    complete_vector_arg(grouping_vars) %>%
    organize()

  output
}
#' Organizes common GLP data by columns and rows and replaces FIPS 1073 with 01073.
#'
#' Columns: MSA, FIPS, city, year, sex, race, baseline, current,
#' Rows: MSA, FIPS, year, sex, race
#'
#' @param df A data frame
#' @export
organize <- function(df) {

  columns <- df %cols_in% c("MSA", "FIPS",
                            "district", "zip", "tract", "block_group", "neighborhood", "block",
                            "year", "sex", "race", "frl_status", "var_type",
                            "city", "variable", "baseline", "current",
                            "line1", "line2", "line3")

  rows <- df %cols_in% c("MSA", "FIPS",
                         "district", "zip", "tract", "block_group", "neighborhood", "block",
                         "variable", "year", "sex", "race", "frl_status", "var_type")


  if("var_type" %in% names(df)) {
    var_type_sort <- c("estimate", "MOE", "percent", "population", "CI")
    var_type_sort <- var_type_sort[var_type_sort %in% unique(df$var_type)]

    df$var_type <- factor(df$var_type, levels = var_type_sort, ordered = TRUE)
  }

  df %<>%
    select(columns, everything()) %>%
    arrange_at(vars(rows))

  if ("FIPS" %in% names(df)){
    df %<>%
      mutate(FIPS = replace(FIPS, FIPS == "1073", "01073"))
  }
  if ("MSA" %in% names(df)) {
    df %<>%
      mutate(MSA = as.character(MSA))
  }

  if("var_type" %in% names(df)) {
    df$var_type <- as.character(df$var_type)
  }

  df
}

#' Check if each row of a data frame is unique
#'
#' Returns
#'
#' @param df A data frame.
#' @export
unique_check <- function(df, other_grouping_vars = "") {
  grouping_vars <- c("MSA", "FIPS", "tract", "neighborhood",
                     "year", "sex", "race", other_grouping_vars)

  grouping_vars <- df %cols_in% grouping_vars

  num_per_group <- df %>%
    group_by_at(grouping_vars) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(n) %>%
    pull(n) %>%
    unique()

  num_per_group
}

#' Check if each row of a data frame is unique
#'
#' Returns
#'
#' @param df A data frame.
#' @export
complete_check <- function(df, other_grouping_vars = "") {
  grouping_vars <- c("MSA", "FIPS", "tract", "neighborhood",
                     "year", "sex", "race", other_grouping_vars)

  grouping_vars <- df %cols_in% grouping_vars

  num_per_group <- df %>%
    complete_vector_arg(grouping_vars) %>%
    group_by_at(grouping_vars) %>%
    summarise(n = n(), .groups = "drop") %>%
    ungroup() %>%
    pull(n) %>%
    unique(n)

  num_per_group
}

#' Return the years in a data frame
#'
#' @param df A data frame.
#' @param var A variable of interest.
#' @param demographic .
#' Defaults to \code{TRUE}.
#' @export
years_in_df <- function(df, var, category = ""){

  if (class(substitute(var)) == "name") {
    var <- deparse(substitute(var))
  }

  df$var <- df[[var]]

  if (category == "") {
    df_subset <- df
  } else if (df_type(df) == "ky_ed") {
    df_subset <- df %>% filter(demographic == category)
  } else if (df_type(df) %in% c("county", "MSA")) {
    if (category %in% c("male", "female")) {
      df_subset <- df %>% filter(sex == category)
    } else if (category == "sex") {
      df_subset <- df %>% filter(sex == "male")
    } else if (category %in% c("white", "black", "hispanic")) {
      df_subset <- df %>% filter(race == category)
    } else if (category == "race") {
      df_subset <- df %>% filter(race == "white")
    }
  }

  results <- df_subset %>%
    group_by(year) %>%
    summarise(pct_na = mean(is.na(var)), .groups = "drop") %>%
    filter(pct_na < 1)

  results$year
}

#' Add a population column to a GLP-format dataframe
#'
#' @param df A data frame
#' @param geog Title of the geography column
#'
#' @export
add_population <- function(df, geog) {
  if ("population" %in% names(df)) {
    warning("Variable 'population' already exists in data frame")
    return(df)
  }

  if (missing(geog)) geog <- df %cols_in% c("MSA", "FIPS", "tract", "neighborhood", "zip")

  if(length(geog) > 1) {
    stop("Too many geography columns. Provide geog argument.")
  }

  # Create a clean, minimal population data frame
  tryCatch({
    pop_df <- switch(df_type(df),
                     "MSA"   = glpdata::population_msa_1yr,
                     "FIPS"  = glpdata::population_county,
                     "tract" = glpdata::population_tract,
                     "nh"    = glpdata::population_nh,
                     "muw"   = glpdata::population_muw,
                     "zip"   = glpdata::population_zip)
  },
  error = function(e){
    stop("Geography not MSA, FIPS, tract, nh, muw, or zip.")
  })

  join_vars <- c(geog, df %cols_in% c("year", "sex", "race"))

  if("year" %not_in% join_vars) pop_df %<>% filter(year == 2018)
  if("race" %not_in% join_vars) pop_df %<>% filter(sex == "total")
  if("sex" %not_in% join_vars)  pop_df %<>% filter(race == "total")

  pop_df %<>% select_at(c(join_vars, "population"))

  df %<>% left_join(pop_df, by = join_vars)

  df
}



#' Create population-adjusted variables
#'
#' @param df A data frame
#' @param ... Variables
#' @param geog The geography
#' @param keep_vars Keep original variables?
#' @param keep_pop Keep population in data frame
#'
#' @export
per_capita_adj <- function(df, ..., pop_var = "population", geog,
                           keep_vars = T, keep_pop = F, other_grouping_vars = "", scale = 1) {

  # Create list of variables from ... argument
  variables <- dplyr:::tbl_at_vars(df, vars(...))
  pop_var <- as.character(substitute(pop_var))

  join_vars <- df %cols_in% c("MSA", "FIPS", "tract", "neighborhood",
                              "year", "race", "sex", other_grouping_vars)

  # Determine geography and other variables to join by
  if (pop_var == "population" & "population" %not_in% names(df)) {
    df %<>% add_population()
  }

  # Join df to population df and divide by population.
  # If keep_vars == TRUE, retain original variables.

  suffix <- case_when(
    scale == 1 ~ "_pp",
    scale %in% c(10, 100, 1000) ~ paste0("_per_", scale),
    scale %in% c(10000, 100000) ~ paste0("_per_", scale / 1000, "k"),
    scale == 1000000 ~ "_per_million")

  if (keep_vars) {
    new_df <- df %>%
      mutate_at(variables, ~ . / .data[[pop_var]] * scale) %>%
      rename_at(variables, ~ paste0(., suffix)) %>%
      select_at(c(join_vars, paste0(variables, suffix)))

    df %<>% bind_df(new_df, by = join_vars)
  } else {
    df %<>%
      mutate_at(variables, ~ . / .data[[pop_var]])
  }

  # If keep_pop == FALSE, remove population variable
  if (!keep_pop) df %<>% select(-population)

  df
}



#' Load GLP-related packages
#'
#' @param graphs Will graphs or maps be made?
#' @export
complete_vector_arg <- function(df, columns, years, keep_empty_groups = FALSE) {

  # Create string to evaluate as function using columns argument.
  function_calls <- paste(c("complete(df", columns), collapse = ", ")

  # If data frame is tract-level, use proper set of tracts and years
  if ("tract" %in% columns) {

    # Filter tract df to counties in data
    tract_df <- glptools::tract00_tract_10 %>%
      filter(str_sub(tract00, 1, 5) %in% str_sub(df$tract, 1, 5))

    # Calculate number of observations by year and years by number of observations
    obs_by_year <- df %>%
      group_by(year) %>%
      filter(var_type == "estimate") %>%
      summarise(n = n(), .groups = "drop")

    n_values <- obs_by_year %>%
      pull(n) %>%
      unique()

    # If there is only one group of tracts and years are not provided, add to function calls and assign to years_10
    if (length(n_values) == 1 & missing(years)) {
      years_10 <- obs_by_year$year

      tracts_10 <- unique(tract_df$tract10)

      function_calls %<>% str_replace("tract,", "tract = tracts_10,")
      # Else if years are provided or there are more than
    } else {

      # If years are provided,, add to function calls and assign to years_00 and years_10
      if (!missing(years)) {
        #function_calls %<>% paste0(", year = ", years)

        years_00 <- eval(parse(text = years[1]))
        years_10 <- eval(parse(text = years[2]))

        # Else if there are two groups of tracts, add to function calls and assign to years_00 and years_10
      } else if (length(n_values) == 2) {
        years_00 <- with(obs_by_year, min(year[n == n_values[1]]):max(year[n == n_values[1]]))
        years_10 <- with(obs_by_year, min(year[n == n_values[2]]):max(year[n == n_values[2]]))

        # Otherwise, stop
      } else if (length(n_values) > 2) {
        stop("More than two groups of Census Tracts in data and no years provided.")
      }

      # create output data frames and tract data frames to reflect combinations of years in the data
      df_00 <- df %>% filter(year <= tail(years_00, 1))
      df_10 <- df %>% filter(year >= years_10[1])

      tracts_00 <- unique(tract_df$tract00)
      tracts_10 <- unique(tract_df$tract10)

      function_calls <-
        c(function_calls %>%
            str_replace("df,", "df_00,") %>%
            str_replace("tract,", "tract = tracts_00,"),
          function_calls %>%
            str_replace("df,", "df_10,") %>%
            str_replace("tract,", "tract = tracts_10,"))
    }
  }

  function_calls %<>% paste0(")")

  # Evaluate and bind function calls

  output <- map_dfr(function_calls, ~ eval(parse(text = .)))

  if (!keep_empty_groups & "sex" %in% names(df) & "race" %in% names(df)) {

    # Summarize combinations of race and sex
    df %<>%
      group_by(sex, race) %>%
      summarise(n = n(), .groups = "drop")

    # If the original data frame doesn't include cross-demographics,
    # remove those groups from the new data frame
    if (!any(df$sex != "total" & df$race != "total")) {
      output %<>%
        filter(sex == "total" | race == "total")
    }
  }
  output
}

