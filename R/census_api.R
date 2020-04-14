#' Create a data frame to use for the Census API
#'
#' @param survey The survey to get variables from.
#'   Either \code{sf3}, \code{acs1}, or \code{acs5}.
#' @param table The table to return variables from.
#' @param age_groups Return variables broken down by age group?
#' @export
build_census_var_df <- function(survey, table,
                                age_groups = F) {

  # Get data frame of all variables
  var_table <- glpdata:::census_api_vars

  # Create vector of tables, including demographic breakdowns
  tables <- table %p% c("", LETTERS[1:9])

  # Filter to survey, table, and demograhpic
  var_table %<>%
    filter(
      str_detect(survey, !!survey),
      table %in% tables,
      race %in% c("total", "black", "white", "hispanic"))

  # Remove data broken down by age group
  # Targets: Under 5, 85 years and over, 55 to 59 years, 65 and 66 years, 20 years
  if (!age_groups) {
    var_table %<>%
      filter(
        str_detect(label, "Under", negate = T),
        str_detect(label, "\\d year", negate = T))
  } else {
    var_table %<>%
      filter(
        (str_detect(label, "Under") |
         str_detect(label, "\\d year")))
  }

  # Keep only estimates and remove mrgin of error
  if (str_detect(survey, "acs")) var_table %<>% filter(str_sub(variable, -1) == "E")

  var_table %<>% select(survey, year, variable, race, sex, label, table)

  # Check if there is one row per year, race, and sex
  num_check <- unique_check(var_table)

  if(length(num_check) != 1 | num_check != 1) warning("Seems fishy")

  var_table
}

#' Get data from the Census API
#'
#' @param var_df A data frame of variables to send to the census API
#'   produced by build_census_var_df
#' @param geog The geography to return variables for
#' @param parallel Use future and furrr?
#' @export
get_census <- function(var_df, geog, parallel = F) {

  if (geog %in% c("FIPS", "MSA")) {

    if (geog == "FIPS") {
      geography <- FIPS_df_two_stl$FIPS
    } else if (geog == "MSA") {
      geography <- MSA_FIPS %>% filter(FIPS != "MERGED") %>% pull(FIPS)
    }

    var_df <- tidyr::crossing(geography, var_df)

    fxn <- function(survey, year, geography, data, ...) {

      output <- tryCatch({
        api <- censusapi::getCensus(
          name = survey,
          vintage = year,
          vars = data$variable,
          regionin = "state:" %p% str_sub(geography, 1, 2),
          region = "county:" %p% str_sub(geography, 3, 5),
          key = Sys.getenv("CENSUS_API_KEY"))

        api %<>% pivot_longer(cols = data$variable)

        if (str_detect(survey, "acs5")) year = year - 2

        output <- data.frame(
          FIPS = geography,
          year = year,
          race = data$race,
          sex = data$sex,
          var = api$value,
          stringsAsFactors = F)
      },
      error = function(cond){
        if (str_detect(survey, "acs5")) year = year - 2

        return(data.frame(
          FIPS = geography,
          year = year,
          race = data$race,
          sex = data$sex,
          var = rep(NA_real_, nrow(data)),
          stringsAsFactors = F))
      })

      output
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

      api %<>% pivot_longer(cols = data$variable)

      if (str_detect(survey, "acs5")) year = year - 2

      output <- data.frame(
        tract = paste0("21111", api$tract),
        year = year,
        race = data$race,
        sex = data$sex,
        var = api$value,
        stringsAsFactors = F)

      output
    }
  } else if (geog == "zip") {
      fxn <- function(survey, year, data, ...) {

        api <- censusapi::getCensus(
          name = survey,
          vintage = year,
          vars = data$variable,
          region = "zip code tabulation area:*",
          key = Sys.getenv("CENSUS_API_KEY"))

        api %<>% pivot_longer(cols = data$variable)

        if (str_detect(survey, "acs5")) year = year - 2

        output <- data.frame(
          zip = api$zip_code_tabulation_area,
          year = year,
          race = data$race,
          sex = data$sex,
          var = api$value,
          stringsAsFactors = F)

        output
      }
    }

  if (geog %in% c("FIPS", "MSA")) {grouping_vars <- c("survey", "geography", "year")}
  else                            {grouping_vars <- c("survey", "year")}

  output <- var_df %>%
    group_by_at(grouping_vars) %>%
    nest()

  if (parallel) {
    future::plan(future::multiprocess)
    output %<>% furrr::future_pmap_dfr(fxn)
  } else {
    output %<>% purrr::pmap_dfr(fxn)
  }

  output
}
