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


  # Keep only estimates and remove mrgin of error
  if (str_detect(survey, "acs")) var_table %<>% filter(str_sub(variable, -1) == "E")

  var_table %<>% select(survey, year, variable, race, sex, age_group, age_low, age_high, label, table)

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
            variable = data$variable,
            stringsAsFactors = F)
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
            tract = paste0(geography, tract),
            year  = if_else(str_detect(survey, "acs5"), year - 2, year),
            race,
            sex,
            age_group,
            age_low,
            age_high,
            value,
            label,
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
          tract = paste0("21111", tract),
          year  = if_else(str_detect(survey, "acs5"), year - 2, year),
          race,
          sex,
          age_group,
          age_low,
          age_high,
          value,
          label,
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
    future::plan(future::multiprocess)
    output %<>% furrr::future_pmap_dfr(fxn)
  } else {
    output %<>% purrr::pmap_dfr(fxn)
  }

  if (!missing(var_name)) output %<>% rename(!!var_name := value)

  output
}
