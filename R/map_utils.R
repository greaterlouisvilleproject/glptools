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
#' @param ... Variables to calculate
#' @param return_name The name of the maps to return. Suffixes will be appended.
#' @param maps The type of maps to return. Defaults to tract, nh, and muw.
#' @param council_MOE Simulate Metro Council Districts? Takes a while.
#'
#' @export
process_map <- function(map_df,
                        ...,
                        return_name = NULL,
                        maps = c("tract", "nh", "muw", "bg", "district"),
                        council_MOE = TRUE) {

  variables <- dplyr:::tbl_at_vars(map_df, vars(...))
  grouping_vars <- map_df %cols_in% c("year","sex", "race")

  # Remove block group from default if the map is tract-level
  if (df_type(map_df) == "tract") maps = setdiff(maps, "bg")

  # # create function based on method
  # fxn <- switch(method,
  #               "percent" = function(x, y) sum(x) / sum(y) * 100,
  #               "mean"    = function(x, y) weighted.mean(x, y),
  #               "sum"     = function(x, y) sum(x),
  #               "median"  = function(x, y) Hmisc::wtd.quantile(x, y, na.rm = T, probs = .5))
  #
  # # If median is selected, remove Airport to prevent errors
  # if (method == "median") map_df %<>% filter(tract != "21111980100")

  # Group by geography and year,
  #   bind to neighborhood labels (if applicable),
  #   and summarise values
  if ("tract" %in% maps) {

    if(df_type(map_df) == "block_group") map_df %<>% mutate(tract = str_sub(block_group, 1, 11))

    df_tract <- map_df %>%
      pivot_longer(cols = variables, names_to = "variable") %>%
      pivot_wider(names_from = var_type, values_from = value) %>%
      group_by_at(c("tract", grouping_vars, "variable")) %>%
      summarise(estimate   = sum(estimate),
                population = sum(population), #/ n() * length(unique(tract)),  Divide by the number of groups
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
                population = sum(population), #/ n() * length(unique(tract)),  Divide by the number of groups
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
                population = sum(population), #/ n() * length(unique(tract)),  Divide by the number of groups
                percent = estimate / population * 100,
                MOE = sqrt(sum(MOE^2)),
                CI = MOE / population * 100,
                .groups = "drop") %>%
      pivot_longer(cols = estimate:CI, names_to = "var_type") %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      select_at(c("neighborhood", grouping_vars, "var_type", variables))
  }

  if ("bg" %in% maps) {
    df_bg <- map_df %>%
      pivot_longer(cols = variables, names_to = "variable") %>%
      pivot_wider(names_from = var_type, values_from = value) %>%
      group_by_at(c("block_group", grouping_vars, "variable")) %>%
      summarise(estimate   = sum(estimate),
                population = sum(population), #/ n() * length(unique(tract)),  Divide by the number of groups
                percent = estimate / population * 100,
                MOE = sqrt(sum(MOE^2)),
                CI = MOE / population * 100,
                .groups = "drop") %>%
      pivot_longer(cols = estimate:CI, names_to = "var_type") %>%
      pivot_wider(names_from = "variable", values_from = "value") %>%
      select_at(c("block_group", grouping_vars, "var_type", variables))
  }

  if ("district" %in% maps) {

    if (council_MOE) {
      df_district <- map_df %>%
        census_to_council(variables)
    } else {
      df_district <- map_df %>%
        census_to_council_direct(variables)
    }
  }


  # Replace Airport values with NAs. If median was selected, bind Airport rows.
  #if (method %in% c("percent", "mean", "sum")) {
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
  if ("bg" %in% maps)   df_bg   %<>% mutate_at(variables,
                                               ~ replace(., var_type %in% c("estimate", "CI") &
                                                           block_group == "211119801001", NA))
  #}

  # Create list to return based on map parameter
  output <-
    purrr::map(maps, ~ paste0("df_", .x) %>% assign(.,get(.))) %>%
    setNames(paste0(return_name, "_", maps)) %>%
    purrr::map(organize)

  output
}

#' Convert census tract or block group data to metro council district data the simple way
#'
#' @param df A data frame from the census
#' @param ... a tidyselection of variables
#'
#' @export
census_to_council_direct <- function(df, ..., geog) {

  variables <- dplyr:::tbl_at_vars(df, vars(...))

  if(missing(geog)) geog <- df_type(df)

  if (geog == "tract") {
    crosswalk <- district_tract
  } else {
    crosswalk <- district_block_group
  }

  crosswalk %<>%
    group_by(across(all_of(geog))) %>%
    mutate(pct_dist = population / sum(population)) %>%
    select(-population) %>%
    ungroup()

  df %<>%
    filter(var_type %in% c("estimate", "population")) %>%
    pivot_vartype_wider(variables) %>%
    left_join(crosswalk, by = geog) %>%
    group_by(variable) %>%
    mutate(across(estimate:population, ~ . * pct_dist)) %>%
    group_by(district, year, sex, race, variable) %>%
    summarise(
      estimate = sum(estimate),
      population = sum(population), .groups = "drop") %>%
    filter(across(estimate, ~!is.na(.))) %>%
    pivot_vartype_longer()

  df

}

#' Convert census block group data to metro council district data using simulations
#'
#' @param df A data frame from the census
#' @param ... a tidyselection of variables
#'
#' @export
census_to_council <- function(df, ..., geog) {

  variables <- dplyr:::tbl_at_vars(df, vars(...))
  grouping_vars <- df %cols_in% c("year", "sex", "race", "age_group")

  if(missing(geog)) geog <- df_type(df)

  if (geog == "tract") {
    crosswalk <- district_tract

    df %<>% filter(tract != "21111980100")
  } else {
    crosswalk <- district_block_group %>%
      select(-tract)

    df %<>% filter(block_group != "211119801001")
  }

  # Summarize block group/tract to district allocations
  crosswalk %<>%
    group_by(across(all_of(geog))) %>%
    mutate(across(population, ~ . / sum(.)))

  # Create data frame of simulated populations for each characteristic in each block group/tract
  simulate_population <- function(df, grouping_vars) {

    # Draw 1000 populations for each variable
    model_population <- function(var, var_type) {
      rnorm(1000, var[var_type == "estimate"], var[var_type == "MOE"] / 1.645)
    }

    output <- map_dfc(df[,df %cols_not_in% grouping_vars], ~model_population(., df$var_type))

    # Replace values under 0 and remove rows with all 0 estimates
    output %<>%
      mutate(across(everything(),
                    ~case_when(. < 0 ~ 0,
                               TRUE ~ .))) %>%
      filter(rowSums(.) != 0) %>%
      mutate(simulation = row_number())

    output
  }

  set.seed(42) # set the seed so the data is the same each time the code is run with the same input data

  block_sims <- df %>%
    group_by(across(all_of(c(geog, grouping_vars)))) %>%
    nest() %>%
    mutate(simulations = purrr::map(data, ~simulate_population(., c(grouping_vars, "var_type")))) %>%
    select(-data) %>%
    unnest(cols = c(simulations)) %>%
    ungroup()

  # Join crosswalk to data frame and summarize data by district for each siulation
  district_sims <- block_sims %>%
    left_join(crosswalk, by = geog) %>%
    mutate(across(all_of(variables), ~ . * population)) %>%
    group_by(across(all_of(c("district", grouping_vars, "simulation")))) %>%
    summarise(across(all_of(variables), ~ sum(.)), .groups = "drop")

  # Summarize data into estimates and margins of error
  council_summary_fxn <- function(df, variables) {
    estimate <- df %>%
      summarise(across(all_of(variables), mean))

    MOE <- df %>%
      summarise(across(all_of(variables), ~sd(.) * 1.645))

    temp <- bind_rows(estimate, MOE)

    bind_cols("var_type" = c("estimate", "MOE"), temp)
  }

  district_summaries <- district_sims %>%
    group_by(across(all_of(c("district", grouping_vars)))) %>%
    nest() %>%
    mutate(results = map(data, ~council_summary_fxn(., variables))) %>%
    select(-data) %>%
    unnest(cols = c(results)) %>%
    ungroup()

  # Get populations back
  district_summaries %<>%
    pivot_vartype_wider(variables) %>%
    group_by(across(all_of(c("district", grouping_vars)))) %>%
    mutate(population = sum(estimate)) %>%
    group_by(across(all_of(c("district", grouping_vars, "variable")))) %>%
    sum_by_var_type()

  district_summaries %<>%
    pivot_vartype_longer()

  district_summaries
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

