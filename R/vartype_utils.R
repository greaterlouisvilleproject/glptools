#' Summarize vars
#'
#' @param df .
#' @param ... Column names to combine.
#' @param drop_groups .
#' @export
sum_by_var_type <- function(df, ..., output_vartypes, drop_groups = TRUE) {

  var_types <- df %cols_in% c("estimate", "population", "MOE", "percent", "CI")

  if (missing(output_vartypes)) {
    output_vartypes <- case_when(
      all(var_types == "estimate") ~ "estimate",
      all(var_types %in% c("estimate", "MOE")) ~ c("estimate", "MOE"),
      all(var_types %in% c("estimate", "population")) ~ c("estimate", "population", "percent"),
      all(c("estimate", "population", "MOE") %in% var_types) ~
        c("estimate", "population", "percent", "MOE", "CI"))
  }

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
pivot_vartype_wider <-function(df, variables) {
  df %>%
    pivot_longer(variables,
                 names_to = "variable",
                 values_to = "value") %>%
    pivot_wider(names_from = "var_type",
                values_from = "value")
}

