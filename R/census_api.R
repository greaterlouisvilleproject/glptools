#' Create a data frame to use for the Census API
#'
#' @param survey The survey to get variables from.
#'   Either \code{sf3}, \code{acs1}, or \code{acs5}.
#' @param table The table to return variables from.
#' @param age_groups Return variables broken down by age group?
#' @param additional_filters A character vector of additional expressions that will
#'   be searched for in the label
#' @export
build_census_var_df <- function(survey, table, age_groups,
                                additional_filters = "") {

  # Get data frame of all variables
  var_table <- glptools:::census_api_vars

  # Create vector of tables, including demographic breakdowns
  tables <- crossing(t = table, l = c("", LETTERS[1:9])) %>%
    mutate(out = paste0(t, l)) %>%
    pull(out)

  # Filter to survey, table, and demograhpic
  var_table %<>%
    filter(
      str_detect(survey, !!survey),
      table %in% tables,
      race %in% c("total", "black", "white", "hispanic"))

  if (length(additional_filters) != 1 | additional_filters[1] != "") {
    var_table %<>% filter(str_detect(label, additional_filters))
  }

  # Remove data broken down by age group
  # Targets: 1 year, Under 5, 85 years and over, 55 to 59 years, 65 and 66 years, 20 years
  if (!missing(age_groups)) {
    var_table %<>% filter(age_group %in% age_groups)
  }


  # Keep only estimates and remove margin of error
  if (str_detect(survey, "acs")) var_table %<>% filter(str_sub(variable, -1) == "E")

  # Add MOE
  var_table_MOE <- var_table %>%
    filter(str_detect(survey, "acs")) %>%
    mutate(
      variable = str_replace(variable, "(?<=_\\d{3})E", "M"),
      var_type = if_else(str_sub(variable, -1) == "M", "MOE", "estimate"))

  var_table %<>%
    mutate(var_type = "estimate") %>%
    bind_rows(var_table_MOE)

  var_table %<>% select(survey, year, variable, race, sex,
                        var_type, age_group, age_low, age_high, label, table)

  # Check if there is one row per year, race, and sex
  unique_check(var_table)

  var_table
}

#' Get data from the Census API
#'
#' @param var_df A data frame of variables to send to the census API
#'   produced by build_census_var_df
#' @param geog The geography to return variables for
#' @param var_name Optional: a name for the value column in the returned data frame
#' @param parallel Use future and furrr?
#' @export
get_census <- function(var_df, geog, var_name, parallel = T) {

  if (geog %in% c("MSA", "FIPS", "Louisville")) {
    fxn <- function(survey, year, geography, data, ...) {

      output <- tryCatch({
        api <- censusapi::getCensus(
          name = survey,
          vintage = year,
          vars = data$variable,
          regionin = "state:" %p% str_sub(geography, 1, 2),
          region = "county:" %p% str_sub(geography, 3, 5),
          key = Sys.getenv("CENSUS_API_KEY"))

        api %<>%
          pivot_longer(cols = data$variable) %>%
          left_join(data, by = c("name" = "variable")) %>%
          transmute(
            FIPS = geography,
            year = if_else(str_detect(survey, "acs5"), year - 2, year),
            race,
            sex,
            age_group,
            age_low,
            age_high,
            value,
            label,
            var_type,
            variable = name)
      },
      error = function(cond){
        data.frame(
          FIPS = geography,
          year = if_else(str_detect(survey, "acs5"), year - 2, year),
          race = data$race,
          sex  = data$sex,
          age_group = data$age_group,
          age_low = data$age_low,
          age_high = data$age_high,
          value = rep(NA_real_, nrow(data)),
          label = data$label,
          var_type = data$var_type,
          variable = data$variable)
      })

      output
    }
  } else if (geog == "tract_all") {
    fxn <- function(survey, year, geography, data, ...) {

      api <- censusapi::getCensus(
        name = survey,
        vintage = year,
        vars = data$variable,
        regionin = paste0("state:", str_sub(geography, 1, 2),
                          "&county:", str_sub(geography, 3, 5)),
        region = "tract:*",
        key = Sys.getenv("CENSUS_API_KEY"))

      api %<>%
        pivot_longer(cols = data$variable) %>%
        left_join(data, by = c("name" = "variable")) %>%
        transmute(
          tract = paste0(geography, str_pad(tract, 6, "right", "0")),
          year  = if_else(str_detect(survey, "acs5"), year - 2, year),
          race,
          sex,
          age_group,
          age_low,
          age_high,
          value,
          label,
          var_type,
          variable = name)
    }
  } else if (geog == "tract") {
    fxn <- function(survey, year, data, ...) {

      api <- censusapi::getCensus(
        name = survey,
        vintage = year,
        vars = data$variable,
        regionin = "state:21&county:111",
        region = "tract:*",
        key = Sys.getenv("CENSUS_API_KEY"))

      api %<>%
        pivot_longer(cols = data$variable) %>%
        left_join(data, by = c("name" = "variable")) %>%
        transmute(
          tract = paste0("21111", str_pad(tract, 6, "right", "0")),
          year  = if_else(str_detect(survey, "acs5"), year - 2, year),
          race,
          sex,
          age_group,
          age_low,
          age_high,
          value,
          label,
          var_type,
          variable = name)
    }
  } else if (geog == "block_group") {
    fxn <- function(survey, year, data, ...) {

      api <- censusapi::getCensus(
        name = survey,
        vintage = year,
        vars = data$variable,
        regionin = "state:21&county:111",
        region = "block group:*",
        key = Sys.getenv("CENSUS_API_KEY"))

      api %<>%
        pivot_longer(cols = data$variable) %>%
        left_join(data, by = c("name" = "variable")) %>%
        transmute(
          block_group = paste0("21111", str_pad(tract, 6, "right", "0"), block_group),
          year  = if_else(str_detect(survey, "acs5"), year - 2, year),
          race,
          sex,
          age_group,
          age_low,
          age_high,
          value,
          label,
          var_type,
          variable = name)
    }
  } else if (geog == "zip") {
    fxn <- function(survey, year, data, ...) {

      api <- censusapi::getCensus(
        name = survey,
        vintage = year,
        vars = data$variable,
        region = "zip code tabulation area:*",
        key = Sys.getenv("CENSUS_API_KEY"))

      api %<>%
        pivot_longer(cols = data$variable) %>%
        left_join(data, by = c("name" = "variable")) %>%
        transmute(
          zip = zip_code_tabulation_area,
          year = if_else(str_detect(survey, "acs5"), year - 2, year),
          race,
          sex,
          age_group,
          age_low,
          age_high,
          value,
          label,
          var_type,
          variable = name)
    }
  }

  if (geog %in% c("MSA", "FIPS", "tract_all", "Louisville")) {

    if (geog == "Louisville") {
      geography <- "21111"
      geog <- "FIPS"
    }
    if (geog %in% c("FIPS", "tract_all")) geography <- FIPS_df_two_stl$FIPS
    if (geog == "MSA")  geography <- MSA_FIPS %>% filter(FIPS != "MERGED") %>% pull(FIPS)

    var_df <- tidyr::crossing(geography, var_df)
    grouping_vars <- c("survey", "geography", "year")
  } else {
    if (geog == "zip") var_df %<>% filter(year == 2000 | year >= 2011) # ZCTA data not available for 2009 or 2010

    grouping_vars <- c("survey", "year")
  }

  output <- var_df %>%
    group_by_at(grouping_vars) %>%
    nest()

  if (parallel) {
    future::plan(future::multisession)
    output %<>% furrr::future_pmap_dfr(fxn)
  } else {
    output %<>% purrr::pmap_dfr(fxn)
  }

  if (!missing(var_name)) output %<>% rename(!!var_name := value)

  # organize columns
  output %<>%
    select(all_of(c(geog, "year", "sex", "race", "var_type")), everything())

  output
}

#' Algorithm to create GLP-style data frame from census downloads
#'
#' @param df A data frame from the census
#' @param var_names Variable names
#' @param cat_var Categorical vari
#' @param output_name Output var name
#'
#' @export
process_census <- function(df,
                           var_names = "value",
                           cat_var,
                           output_name,
                           age_groups = "all",
                           age_group_wide = FALSE,
                           sum_to_100 = TRUE) {


  # PREP DATA

  # Get geography and grouping variables
  geog <- df_type(df)
  grouping_vars <- df %cols_in% c(df_type(df), "year", "race", "sex")

  # generate levels of cat_var for later use
  cat_levels <- unique(df[[cat_var]])
  cat_levels <- setdiff(cat_levels, c("total", NA_character_))

  # Remove variable column if it's here to prevent later errors with pivot_vartype_longer
  df %<>%
    filter(!is.na(.data[[cat_var]]))

  # If data is county-level, replace St. Louis FIPS codes with MERGED
  if (geog == "FIPS") df %<>% stl_merge(just_replace_FIPS = TRUE)

  # SUMMARIZE DATA

  # Summarize data within each age group and level of cat_vat
  for (a in age_groups) {

    temp <- df

    # Subset to age group

    if (a == "all") {
      if("all" %in% df$age_group) temp %<>% filter(age_group == "all")

    } else {
      ages <- str_split(a, "_", n = 2, simplify = TRUE) %>% as.numeric()

      ages[1] <- replace_na(ages[1], 0)
      ages[2] <- replace_na(ages[2], Inf)

      temp %<>%
        filter(
          age_low  >= ages[1],
          age_high <= ages[2])
    }

    # Create population data by either using the "total" indicator or summing all categories
    if("total" %in% df[[cat_var]]) {
      temp_pop <- temp %>%
        filter(.data[[cat_var]] == "total")

      temp %<>%
        filter(.data[[cat_var]] != "total")
    } else {
      temp_pop <- temp
    }

    temp_pop %<>%
      filter(var_type == "estimate") %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarize(
        across(var_names, ~ sum(.)),
        var_type = "population",
        .groups = "drop") %>%
      crossing(!!cat_var := temp[[cat_var]]) # Replicate population for each level of cat_var

    # Join population to data, summarize, and add age group
    temp %<>%
      filter(!is.na(.data[[cat_var]])) %>%
      bind_rows(temp_pop) %>%
      select(all_of(c(grouping_vars, cat_var, "var_type", "value"))) %>%
      pivot_vartype_wider(var_names) %>%
      group_by(across(all_of(c(grouping_vars, cat_var)))) %>%
      #mutate(row_num = row_number()) %>%  # Needed to prevent pivot from creating list columns when > 1 observation per group
      sum_by_var_type(value) %>%
      mutate(age_group = a)

    output <- assign_row_join(output, temp)
  }

  df <- output

  # CLEAN AND TIDY DATA

  # If vat_var is logical, keep only the TRUE values and replace values of cat_var with output_name
  if (typeof(cat_levels) == "logical") {
    df %<>%
      filter(.data[[cat_var]]) %>%
      mutate(!!cat_var := output_name)
  }

  # Pivot var_type from column names to the var_type column
  df %<>%
    pivot_vartype_longer(names_from_col = cat_var)

  var_cols <- df %cols_not_in% c(grouping_vars, "var_type", "age_group")

  # If age_group_wide, spread age groups across columns
  if (age_group_wide & length(age_groups) == 1) {

    df %<>%
      rename_with(~paste0(., "_", age_groups), var_cols) %>%
      select(-age_group)

  } else if (age_group_wide) {

    df %<>%
      pivot_wider(id_cols = c(grouping_vars, "var_type"),
                  names_from = age_group,
                  values_from = var_cols)

    # Drop "_all" from group names because it's implied
    if(any(str_detect(names(df), "_all$"))) {
      df %<>% rename_with(~str_remove(., "_all$"), str_detect(names(df), "_all$"))
    }
  } else if (length(age_groups) == 1) {
    df %<>%
      select(-age_group)
  }

  df %<>%
    total_demographics(df %cols_not_in% c(grouping_vars, "var_type", "age_group"),
                       other_grouping_vars = "age_group") %>%
    organize()

  df

}
