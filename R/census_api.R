#' Create a data frame to use for the Census API
#'
#' @param survey The survey to get variables from.
#'   Either \code{sf3}, \code{acs1}, or \code{acs5}.
#' @param table The table to return variables from.
#' @param age_groups Return variables broken down by age group?
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
  var_table_moe <- var_table %>%
    filter(str_detect(survey, "acs")) %>%
    mutate(
      variable = str_replace(variable, "(?<=_\\d{3})E", "M"),
      est_moe = if_else(str_sub(variable, -1) == "M", "MOE", "estimate"))

  var_table %<>%
    mutate(est_moe = "estimate") %>%
    bind_rows(var_table_moe)

  var_table %<>% select(survey, year, variable, est_moe,
                        race, sex, age_group, age_low, age_high, label, table)

  # Check if there is one row per year, race, and sex
  unique_check(var_table)

  var_table
}

#' Get data from the Census API
#'
#' @param var_df A data frame of variables to send to the census API
#'   produced by build_census_var_df
#' @param geog The geography to return variables for
#' @param parallel Use future and furrr?
#' @export
get_census <- function(var_df, geog, var_name, parallel = T, label = F, var = F) {

  if (geog %in% c("MSA", "FIPS")) {
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
            est_moe,
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
            est_moe = data$est_moe,
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
            est_moe,
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
          est_moe,
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
          est_moe,
          variable = name)
    }
  }

  if (geog %in% c("MSA", "FIPS", "tract_all")) {
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
process_census <- function(df, var_names = "value", cat_var, output_name, age_groups = "all",
                           output_percent = TRUE, output_population = FALSE, sum_to_100 = TRUE) {

  # Get geography and grouping variables
  geog <- df_type(df)
  grouping_vars <- df %cols_in% c(df_type(df), "year", "race", "sex", cat_var)

  # If more than one age group, create column names using age groups. Drop "_all" from group names.
  if (length(age_groups) > 1) {
    output_vars <- paste0(output_name, "_", age_groups)
  } else {
    output_vars <- output_name
  }
  output_vars <- str_replace(output_vars, "_all", "")

  # Summarize data within cat_var categories by grouping vars, including age group
  for (a in age_groups) {

    temp <- df

    # Subset data to age group
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

    # Filter out NA cat_var data, summarise data, and add age group to data frame
    temp %<>%
      filter(across(!!cat_var, ~ !is.na(.))) %>%
      group_by(across(grouping_vars))

    temp_est <- temp %>%
      filter(est_moe == "estimate") %>%
      summarise(across(var_names, ~ sum(.)), .groups = "drop") %>%
      mutate(
        age_group = a,
        var_type = "estimate")

    temp_moe <- temp %>%
      filter(est_moe == "MOE") %>%
      summarise(across(var_names, ~ sqrt(sum(. * .))), .groups = "drop") %>%
      mutate(
        age_group = a,
        var_type = "MOE")

    temp <- bind_rows(temp_est, temp_moe)

    output <- assign_row_join(output, temp)
  }

  df <- output

  # If data is county-level, merge St. Louis
  if (geog == "FIPS") {
    df %<>% stl_merge(var_names, other_grouping_vars = c(cat_var, "age_group"))
  }

  # Spread age groups across columns
  df %<>%
    pivot_wider(id_cols = c(grouping_vars, "var_type"),
                names_from = age_group,
                values_from = var_names,
                names_glue = paste0(output_name, "_{age_group}"))

  # If more than one age group, create column names using age groups. Drop "_all" from group names.
  if (length(age_groups) == 1) {
    df %<>% rename_with(~str_remove(., paste0("_", age_groups)))
  }
  df %<>%
    rename_if(str_detect(names(df), "_all"), ~str_remove(., "_all")) %>%
    select(all_of(c(grouping_vars, "var_type", output_vars)))

  # Create totals
  df %<>% total_demographics(output_vars, other_grouping_vars = cat_var)

  # Conserve population data is it will be resummarised for other map geographies
  df_pop <- df %>%
    filter(var_type == "estimate") %>%
    group_by(across(c(geog, "year", "race", "sex"))) %>%
    mutate(
      across(output_vars, ~ sum(.), .groups = "drop"),
      var_type = "population")

  # Calculate percentages
  if (output_percent) {
    if (sum_to_100) {
      df_pct <- df %>%
        filter(var_type == "estimate") %>%
        group_by(across(c(geog, "year", "race", "sex"))) %>%
        mutate(
          across(output_vars, ~ . / sum(.) * 100),
          var_type = "percent")

      df %<>% bind_rows(df_pct)
    }
    else df %<>% mutate(across(output_vars, ~ . / .[!.data[[cat_var]]] * 100))
  }

  # filter and remove cat_var if logical
  if (typeof(df[[cat_var]]) == "logical") {
    df %<>%
      bind_rows(df_pop) %>%
      filter(across(cat_var, ~.)) %>%
      select(all_of(c(geog, "year", "race", "sex", "var_type", output_vars))) %>%
      mutate(!!cat_var := TRUE)
  } else {
    df %<>%
      bind_rows(df_pop) %>%
      select(all_of(c(geog, "year", "race", "sex", "var_type", cat_var, output_vars)))
  }

  df %<>%
    rename_if(str_detect(names(df), "_0_"),
              ~ str_replace(., "_0_\\d*",
                            paste0("_under_", as.numeric(str_extract(., "(?<=_0_)\\d*")) + 1)))

  output_vars %<>% str_replace(., "_0_\\d*",
                               paste0("_under_", as.numeric(str_extract(., "(?<=_0_)\\d*")) + 1))

  df_ci <- df %>%
    group_by(across(grouping_vars)) %>%
    summarise(
      across(output_vars, ~ .[var_type == "MOE"] / .[var_type == "population"] * 100),
      var_type="CI",
      .groups = "drop")

  df %<>% bind_rows(df_ci)

  if (typeof(df[[cat_var]]) != "logical") {
    df %<>%
      pivot_wider(names_from = cat_var, values_from = output_name)
  } else {
    df %<>% select(-cat_var)
  }

  df %<>% organize()

  df
}
