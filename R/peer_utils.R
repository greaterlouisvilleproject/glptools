#' Subset a data frame containing MSA or county data to peer cities and add current and baseline peer data.
#'
#' @param df A data frame containing the column MSA or FIPS
#' @param add_info Either \code{TRUE} or \code{FALSE}.
#'   Add information columns (city, baseline, and current) to data frame? Defaults to \code{TRUE}.
#' @param subset_to_peers Either \code{TRUE} or \code{FALSE}.
#'   Subset the data to peer geographies? Defaults to \code{TRUE}.
#' @param geog Level of gegraphy to use for adding information and filtering.
#'   Can be \code{FIPS} or \code{MSA}
#'   Defaults to the MSA if present, followed by the FIPS column if present.
#'   (To keep MSA-level data using FIPS as the ID column, geog should be \code{MSA}.)
#' @param additional_geogs A vector of additional MSA or FIPS codes to include.
#'
#' @export pull_peers
pull_peers <- function(df, add_info = F, subset_to_peers = T, geog = "", additional_geogs = ""){

  # If no geography provided, use MSA column. If no MSA column, use FIPS column.
  if (geog == ""){
    if ("MSA" %in% names(df)) geog <- "MSA"
    else if ("FIPS" %in% names(df)) geog <- "FIPS"
  }
  if(geog == "") stop("MSA and FIPS columns are missing from the data frame.")

  # Ensure the Brimingham FIPS code is five digits and that the MSA column is of type character
  if ("MSA" %in% names(df))  df %<>% mutate(MSA = MSA %>% as.character())
  if ("FIPS" %in% names(df)) df %<>% mutate(FIPS = FIPS %>% as.character %>% replace(. == "1073", "01073"))

  # Add information columns
  if (add_info) {
    if      ("MSA" %in% names(df))  df %<>% left_join(MSA_df,  by = "MSA")
    else if ("FIPS" %in% names(df)) df %<>% left_join(FIPS_df, by = "FIPS")
  }

  # subset to peers based on geog
  if(subset_to_peers) {
    if (geog == "FIPS") df %<>% filter(FIPS %in% c(FIPS_df$FIPS, additional_geogs))
    if (geog == "MSA") {
      if ("MSA" %in% names(df)) {
        df %<>% filter(MSA %in% c(MSA_FIPS$MSA, additional_geogs))
      } else {
        df %<>% filter(FIPS %in% c(MSA_FIPS$FIPS, additional_geogs))
      }
    }
    if (geog == "MSA_2012") {
      if ("MSA" %in% names(df)) {
        df %<>% filter(MSA %in% c(MSA_FIPS_2012$MSA, additional_geogs))
      } else {
        df %<>% filter(FIPS %in% c(MSA_FIPS_2012$FIPS, additional_geogs))
      }
    }
  }
  df %<>% organize()

  df
}

#' Combines rows of data from the two St. Louis counties into one.
#'
#' @param df_original A data frame.
#' @param ... Column names to combine.
#' @param weight_var A variable to use when weighting the counties. Defaults to \code{population}.
#' @param method "mean" or "sum". Defaults to mean.
#' @export
stl_merge <- function(df, ..., weight_var = "", method = "mean", simple = F,
                      other_grouping_vars = "", just_replace_FIPS = F, keep_counties = F){

  weight_var <- as.character(substitute(weight_var))
  variables <- dplyr:::tbl_at_vars(df, vars(...))
  var_types <- unique(df$var_type)
  grouping_vars <- c("FIPS", "year", "sex", "race", other_grouping_vars)

  if (just_replace_FIPS) return(df %>% mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")))

  # If simple is true, assign overall populations from 2018 as weights
  if (simple) {

    weight_var <- "weights"

    df %<>%
      mutate(
        weights = case_when(
          FIPS == "29189" ~ 996726,
          FIPS == "29510" ~ 308626,
          TRUE ~ 1))
  }

  # If no weight variable supplied and one is needed, read in total population and join to df
  if(weight_var == "" & method == "mean" & !just_replace_FIPS){

    weight_var <- "population"

    if("population" %not_in% names(df)){

      df %<>% add_population()
    }
  }

  if (keep_counties) {
    df_stl <- df %>%
      select(any_of(c(grouping_vars, variables))) %>%
      filter(FIPS %in% c("29189", "29510"))
  }

  if("var_type" %in% names(df)) {
    df %<>%
      pivot_longer(cols = variables, names_to = "variable") %>%
      pivot_wider(names_from = var_type, values_from = value) %>%
      mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")) %>%
      group_by(across(c(df %cols_in% grouping_vars, "variable"))) %>%
      sum_by_var_type() %>%
      pivot_longer(cols = . %cols_in% c("estimate", "population", "MOE", "percent", "CI"),
                   names_to = "var_type") %>%
      pivot_wider(names_from = "variable", values_from = "value")
  } else {

    #For each variable to be weighted, create a new df of the applicable variables
    for(v in variables){

      temp <- df %>%
        mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")) %>%
        group_by(across(c(df %cols_in% grouping_vars)))

      if (method == "mean") temp %<>% summarise(!!v := weighted.mean(.data[[v]], .data[[weight_var]]), .groups = "drop")
      else if (method == "max")  temp %<>% summarise(!!v := max(.data[[v]], na.rm = TRUE), .groups = "drop")
      else if (method == "min")  temp %<>% summarise(!!v := min(.data[[v]], na.rm = TRUE), .groups = "drop")
      else if (method == "sum")  temp %<>% summarise_at(v, sum, .groups = "drop")

      output <- assign_col_join(output, temp, by = grouping_vars)

    }

    df <- output
  }

  if (keep_counties) df %<>% bind_rows(df_stl)

  df %<>% organize()

  df
}

#' Group data by MSA and summarise
#'
#' @param df A data frame containing the column MSA
#' @export
sum_FIPS_to_MSA <- function(df, ..., other_grouping_vars = "") {

  variables <- dplyr:::tbl_at_vars(df, vars(...))

  grouping_vars <- c("MSA", "year", "sex", "race", other_grouping_vars)

  df %<>%
    left_join(MSA_FIPS, by = "FIPS") %>%
    select(-FIPS) %>%
    group_by_at(. %cols_in% grouping_vars) %>%
    summarise_at(variables, sum) %>%
    ungroup() %>%
    filter(!is.na(MSA))
}

#' Adjust data for cost of living and inflation
#'
#'
#' @param df A data frame.
#' @param ... Variables to adjust.
#' @param remove_calc Whether to remove the columns \code{rpp_index} and \code{cpi_index} after adjustments.
#' Defaults to \code{TRUE}.
#' @export
COLA <- function(df, ..., base_year = 2019, remove_calc = TRUE, inflation = T, rpp = T){
  variables <- dplyr:::tbl_at_vars(df, vars(...))

  geog <- df_type(df)

  COLA_df %<>%
    group_by(FIPS) %>%
    mutate(base_cpi = cpi[year == base_year],
           cpi_index = base_cpi/cpi) %>%
    ungroup() %>%
    select(-cpi, -base_cpi)

  if (geog == "MSA") {
    COLA_df %<>%
      left_join(MSA_FIPS, by = "FIPS") %>%
      select(-FIPS)
  } else if (geog %in% c("tract", "nh")) {
    COLA_df %<>%
      filter(FIPS == "21111") %>%
      select(-FIPS)
  }
  df %<>% left_join(COLA_df, by = COLA_df %cols_in% c(geog, "year"))

  if (inflation & rpp) {
    df %<>% mutate_at(vars(variables), ~ . * rpp_index * cpi_index)
  } else if (inflation & !rpp) {
    df %<>% mutate_at(vars(variables), ~ . * cpi_index)
  } else if (!inflation & rpp) {
    df %<>% mutate_at(vars(variables), ~ . * rpp_index)
  }

  if (remove_calc) df %<>% select(-rpp_index, -cpi_index)

  df
}
