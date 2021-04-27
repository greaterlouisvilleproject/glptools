#' Summarize vars
#'
#' @param df .
#' @param ... Column names to combine.
#' @param drop_groups .
#' @export
sum_by_var_type <- function(df, ..., output_vartypes, drop_groups = TRUE) {

  if("var_type" %in% names(df)) {
    df %<>% pivot_vartype_wider(...)
  }

  # Which var_types are in the data frame?
  var_types <- df %cols_in% c("estimate", "population", "MOE", "percent", "CI")

  # If no var_types are specified, set output_vartypes to all potential combinations
  #   based on what is available in the data
  if (missing(output_vartypes)) {
    pick_num <- case_when(
      all(var_types == "estimate")                           ~ 1,
      all(var_types %in% c("estimate", "MOE"))               ~ 2,
      all(var_types %in% c("estimate", "population"))        ~ 3,
      all(c("estimate", "population", "MOE") %in% var_types) ~ 4)

    output_vartypes <- switch(pick_num,
                              "estimate",
                              c("estimate", "MOE"),
                              c("estimate", "population", "percent"),
                              c("estimate", "population", "percent", "MOE", "CI"))
  }

  # Use map functions if duplicate rows have created list columns, otherwise use regular functions

  if (typeof(df[["estimate"]]) == "list") {

    if (all(output_vartypes == "est")) {
      df %<>%
        summarise(
          estimate   = map_dbl(estimate, sum),
          .groups = "keep")
    } else if (all(output_vartypes %in% c("estimate", "MOE"))) {
      df %<>%
        summarise(
          estimate   = map_dbl(estimate, sum),
          MOE        = map_dbl(MOE, ~sqrt(sum(.^2))),
          .groups = "keep")
    } else if (all(output_vartypes %in% c("estimate", "population"))) {
      df %<>%
        summarise(
          estimate   = map_dbl(estimate, sum),
          population = map_dbl(population, sum),
          .groups = "keep")
    } else if (all(output_vartypes %in% c("estimate", "population", "percent"))) {
      df %<>%
        summarise(
          estimate   = map_dbl(estimate, sum),
          population = map_dbl(population, sum),
          percent    = estimate / population * 100,
          .groups = "keep")
    } else if (all(output_vartypes %in% c("estimate", "population", "percent", "MOE", "CI"))) {
      df %<>%
        summarise(
          estimate   = map_dbl(estimate, sum),
          population = map_dbl(population, sum),
          percent    = estimate / population * 100,
          MOE        = map_dbl(MOE, ~sqrt(sum(.^2))),
          CI         = MOE / population * 100,
          .groups = "keep")
    }
  } else {
    if (all(output_vartypes == "est")) {
      df %<>%
        summarise(
          estimate   = sum(estimate),
          .groups = "keep")
    } else if (all(output_vartypes %in% c("estimate", "MOE"))) {
      df %<>%
        summarise(
          estimate   = sum(estimate),
          MOE        = sqrt(sum(MOE^2)),
          .groups = "keep")
    } else if (all(output_vartypes %in% c("estimate", "population"))) {
      df %<>%
        summarise(
          estimate   = sum(estimate),
          population = sum(population),
          .groups = "keep")
    } else if (all(output_vartypes %in% c("estimate", "population", "percent"))) {
      df %<>%
        summarise(
          estimate   = sum(estimate),
          population = sum(population),
          percent    = estimate / population * 100,
          .groups = "keep")
    } else if (all(output_vartypes %in% c("estimate", "population", "percent", "MOE", "CI"))) {
      df %<>%
        summarise(
          estimate   = sum(estimate),
          population = sum(population),
          percent    = estimate / population * 100,
          MOE        = sqrt(sum(MOE^2)),
          CI         = MOE / population * 100,
          .groups = "keep")
    }
  }

  if (drop_groups) df %<>% ungroup()
}

#' Convert census tract data to metro council district data
#'
#' @export
pivot_vartype_longer <- function(df,
                                 names_from_col = "variable",
                                 values_from_col = "value") {
  df %>%
    pivot_longer(df %cols_in% c("estimate", "population", "MOE", "percent", "CI"),
                 names_to = "var_type",
                 values_to = "value") %>%
    pivot_wider(names_from = names_from_col, values_from = values_from_col)
}

#' Convert census tract data to metro council district data
#'
#' @export
pivot_vartype_wider <- function(df, ...) {
  variables <- dplyr:::tbl_at_vars(df, vars(...))

  df %>%
    pivot_longer(all_of(variables),
                 names_to = "variable",
                 values_to = "value") %>%
    pivot_wider(names_from = "var_type",
                values_from = "value")
}


