#' Create tract, nh, and muw map files from a map data frame
#'
#' @param map_df A data frame of census tracts with a 20-digit ID colum
#' @param variables A character vector of variables
#' @param pop The population variable
#' @param method The method used to aggregate tracts.
#' \code{percent} adds counts and populations to create a percentage
#' \code{mean} takes values and weights to create a weighted mean
#' \code{sum} adds counts and ignores the populations
#' \code{median} creates a weighted median
#' @param return_name The name of the maps to return. Suffixes will be appended.
#' @param maps The type of maps to return. Defaults to tract, nh, and muw.
#'
#' @export
process_map_og <- function(map_df, ..., pop, pop_adjust = F, return_name = NULL,
                           method = "mean", maps = c("tract", "nh", "muw"), keep_pop = FALSE) {

  variables <- dplyr:::tbl_at_vars(map_df, vars(...))
  grouping_vars <- map_df %cols_in% c("year","sex", "race", "var_type")

  if (missing(pop)) {
    map_df %<>% add_population()
    pop <- "population"
  } else {
    pop <- as.character(substitute(pop))
  }

  # create function based on method
  fxn <- switch(method,
                "percent" = function(x, y) sum(x) / sum(y) * 100,
                "mean"    = function(x, y) weighted.mean(x, y),
                "sum"     = function(x, y) sum(x),
                "median"  = function(x, y) Hmisc::wtd.quantile(x, y, na.rm = T, probs = .5))

  # If median is selected, remove Airport to prevent errors
  if (method == "median") map_df %<>% filter(tract != "21111980100")

  # Group by geography and year,
  #   bind to neighborhood labels (if applicable),
  #   and summarise values
  if ("tract" %in% maps) {
    df_tract <- map_df %>%
      group_by(across(c("tract", grouping_vars))) %>%
      mutate(across(variables, ~ fxn(., .data[[pop]]))) %>%
      select(all_of(c("tract", grouping_vars, variables))) %>%
      ungroup()
  }

  if (method %in% c("percent", "mean", "median")) {
    map_df %>%filter(pop != 0)
  }

  if ("nh" %in% maps) {
    df_nh <- map_df %>%
      left_join(nh_tract, by = "tract") %>%
      group_by(across(c("neighborhood", grouping_vars))) %>%
      summarise(across(variables, ~ fxn(., .data[[pop]])), .groups = "drop")
  }

  if ("muw" %in% maps) {
    df_muw <- map_df %>%
      left_join(muw_tract, by = "tract") %>%
      group_by(across(c("neighborhood", grouping_vars))) %>%
      summarise(across(variables, ~ fxn(., .data[[pop]])), .groups = "drop")
  }


  # Replace Airport values with NAs. If median was selected, bind Airport rows.
  if (method %in% c("percent", "mean", "sum")) {
    if ("tract" %in% maps) df_tract %<>% mutate_at(variables,
                                                   ~ replace(., tract == "21111980100", NA))
    if ("nh" %in% maps)    df_nh    %<>% mutate_at(variables,
                                                   ~ replace(., neighborhood == "Airport", NA))
    if ("muw" %in% maps)   df_muw   %<>% mutate_at(variables,
                                                   ~ replace(., neighborhood == "Airport", NA))
  } else if (method == "median") {
    if ("tract" %in% maps) df_tract %<>% complete(nesting(year), tract = "21111980100")
    if ("nh" %in% maps)    df_nh    %<>% complete(nesting(year), neighborhood = "Airport")
    if ("muw" %in% maps)   df_muw   %<>% complete(nesting(year), neighborhood = "Airport")
  }

  if (pop_adjust) {
    if ("tract" %in% maps) df_tract %<>% per_capita_adj(variables)
    if ("nh" %in% maps)    df_nh    %<>% per_capita_adj(variables)
    if ("muw" %in% maps)   df_muw   %<>% per_capita_adj(variables)
  }

  # Create list to return based on map parameter
  output <-
    purrr::map(maps, ~ paste0("df_", .x) %>% assign(.,get(.))) %>%
    setNames(paste0(return_name, "_", maps)) %>%
    purrr::map(organize)

  output
}

#' Create tract, nh, and muw map files from a map data frame
#'
#' @param map_df A data frame of census tracts with a 20-digit ID colum
#' @param variables A character vector of variables
#' @param pop The population variable
#' @param method The method used to aggregate tracts.
#' \code{percent} adds counts and populations to create a percentage
#' \code{mean} takes values and weights to create a weighted mean
#' \code{sum} adds counts and ignores the populations
#' \code{median} creates a weighted median
#' @param return_name The name of the maps to return. Suffixes will be appended.
#' @param maps The type of maps to return. Defaults to tract, nh, and muw.
#'
#' @export
process_map <- function(map_df, ..., pop, pop_adjust = F, return_name = NULL,
                        method = "mean", maps = c("tract", "nh", "muw"), keep_pop = FALSE) {

  variables <- dplyr:::tbl_at_vars(map_df, vars(...))
  grouping_vars <- map_df %cols_in% c("year","sex", "race")

  # create function based on method
  fxn <- switch(method,
                "percent" = function(x, y) sum(x) / sum(y) * 100,
                "mean"    = function(x, y) weighted.mean(x, y),
                "sum"     = function(x, y) sum(x),
                "median"  = function(x, y) Hmisc::wtd.quantile(x, y, na.rm = T, probs = .5))

  # If median is selected, remove Airport to prevent errors
  if (method == "median") map_df %<>% filter(tract != "21111980100")

  # Group by geography and year,
  #   bind to neighborhood labels (if applicable),
  #   and summarise values
  if ("tract" %in% maps) {
    df_tract <- map_df %>%
      pivot_longer(cols = variables, names_to = "variable") %>%
      pivot_wider(names_from = var_type, values_from = value) %>%
      group_by_at(c("tract", grouping_vars, "variable")) %>%
      summarise(estimate   = sum(estimate),
                population = sum(population) / n() * length(unique(tract)), # Divide by the number of groups
                percent = estimate / population * 100,
                MOE = sqrt(sum(MOE^2)),
                CI = MOE / population * 100,
                .groups = "drop") %>%
      pivot_longer(cols = estimate:CI, names_to = "var_type") %>%
      pivot_wider(names_from = "variable", values_from = "value") %>%
      select_at(c("tract", grouping_vars, "var_type", variables))
  }

  if ("nh" %in% maps) {
    df_nh <- map_df %>%
      left_join(nh_tract, by = "tract") %>%
      pivot_longer(cols = variables, names_to = "variable") %>%
      pivot_wider(names_from = var_type, values_from = value) %>%
      group_by_at(c("neighborhood", grouping_vars, "variable")) %>%
      summarise(estimate   = sum(estimate),
                population = sum(population) / n() * length(unique(tract)), # Divide by the number of groups
                percent = estimate / population * 100,
                MOE = sqrt(sum(MOE^2)),
                CI = MOE / population * 100,
                .groups = "drop") %>%
      pivot_longer(cols = estimate:CI, names_to = "var_type") %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      select_at(c("neighborhood", grouping_vars, "var_type", variables))
  }

  if ("muw" %in% maps) {
    df_muw <- map_df %>%
      left_join(muw_tract, by = "tract") %>%
      pivot_longer(cols = variables, names_to = "variable") %>%
      pivot_wider(names_from = var_type, values_from = value) %>%
      group_by_at(c("neighborhood", grouping_vars, "variable")) %>%
      summarise(estimate   = sum(estimate),
                population = sum(population) / n() * length(unique(tract)), # Divide by the number of groups
                percent = estimate / population * 100,
                MOE = sqrt(sum(MOE^2)),
                CI = MOE / population * 100,
                .groups = "drop") %>%
      pivot_longer(cols = estimate:CI, names_to = "var_type") %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      select_at(c("neighborhood", grouping_vars, "var_type", variables))
  }

  # Replace Airport values with NAs. If median was selected, bind Airport rows.
  if (method %in% c("percent", "mean", "sum")) {
    if ("tract" %in% maps) df_tract %<>% mutate_at(variables,
                                                   ~ replace(.,
                                                             var_type %in% c("estimate", "CI") &
                                                               tract == "21111980100", NA))
    if ("nh" %in% maps)    df_nh    %<>% mutate_at(variables,
                                                   ~ replace(., var_type %in% c("estimate", "CI") &
                                                               neighborhood == "Airport", NA))
    if ("muw" %in% maps)   df_muw   %<>% mutate_at(variables,
                                                   ~ replace(., var_type %in% c("estimate", "CI") &
                                                               neighborhood == "Airport", NA))
  }

  # Create list to return based on map parameter
  output <-
    purrr::map(maps, ~ paste0("df_", .x) %>% assign(.,get(.))) %>%
    setNames(paste0(return_name, "_", maps)) %>%
    purrr::map(organize)

  output
}


#' Convert census tract data to metro council district data
#'
#' @param df A data frame from the census
#' @param var_names Variable names
#' @param cat_var Categorical vari
#' @param output_name Output var name
#'
#' @export
tract_to_council <- function(df, ...,
                             method = "percent",
                             weight_var = "total") {

  variables <- dplyr:::tbl_at_vars(df, vars(...))

  if(method == "percent"){
    crosswalk <- district_tract %>%
      group_by(district) %>%
      mutate(across(total:asian, ~ . / sum(.))) %>%
      ungroup()
  } else if(method == "count") {
    crosswalk <- district_tract %>%
      group_by(tract) %>%
      mutate(across(total:asian, ~ . / sum(.))) %>%
      ungroup()
  }

  df %<>%
    left_join(crosswalk, by = "tract") %>%
    mutate(across(variables, ~ . * .data[[weight_var]])) %>%
    group_by(district) %>%
    summarise(across(variables, ~ sum(.)), .groups = "drop") %>%
    filter(across(variables, ~!is.na(.)))

  df

}

#' Transform data from 2000 census tracts to 2010 census tracts
#'
#' @export
tract_00_to_10 <- function(df, years, ..., other_grouping_vars = "") {

  id_cols <- df %cols_in% c("FIPS", "year", "sex", "race", other_grouping_vars)

  df00 <- df %>%
    filter(year %in% years) %>%
    left_join(tract00_tract_10, by = c("tract" = "tract00")) %>%
    group_by(across(c("tract10", id_cols))) %>%
    summarise(across(..., ~ sum(. * percent / 100)), .groups = "drop") %>%
    rename(tract = tract10)

  df10 <- df %>% filter(year %not_in% years)

  bind_rows(df00, df10) %>%
    organize()
}

#' Transform data from 2010 census tracts to 2000 census tracts
#'
#' @export
tract_10_to_00 <- function(df, years, ..., other_grouping_vars = "") {

  id_cols <- df %cols_in% c("FIPS", "year", "sex", "race", other_grouping_vars)

  df10 <- df %>%
    filter(year %in% years) %>%
    left_join(tract00_tract_10, by = c("tract" = "tract10")) %>%
    group_by(across(c("tract00", id_cols))) %>%
    summarise(across(..., ~ sum(. * percent / 100)), .groups = "drop") %>%
    rename(tract = tract00)

  df00 <- df %>% filter(year %not_in% years)

  bind_rows(df00, df10) %>%
    organize()
}

