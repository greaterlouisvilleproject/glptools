#' Not in
#'
#' @export
`%not_in%` <- function (x, table) match(x, table, nomatch = 0L) == 0L


#' paste0(a, b)
#'
#' @export
`%p%` <- function (a, b) paste0(a, b)

#' Returns any variables in the vector
#'   that are columns in the data frame
#'   in the order the appear in the columns vector
#'
#' @name not_in
#' @export
`%cols_in%` <- function (df, columns) columns[columns %in% names(df)]

#' Returns any variables in the vector
#'   that are NOT columns in the data frame.
#'
#' @name not_in
#' @export
`%cols_not_in%` <- function (df, columns) names(df)[names(df) %not_in% columns]

#' Calculate the rolling mean of a vector
#'
#' @param x A vector
#' @param r The number of years to average
#' @return A vector of rolling averages
#' @export
rollmeanr <- function(x, r){
  n <- length(x)
  y <- rep(NA, n)

  dif <- floor(r / 2)

  for(i in (1 + dif):(n - dif)){
    y[i] <- mean(x[(i - dif):(i + dif)])
  }

  y
}

#' Normalize a vector
#'
#' @param x A numeric vector
#' @return A numeric vector of z-scores
#' @export
norm_z <- function(x){
  z <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
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
pull_peers <- function(df, add_info = T, subset_to_peers = T, geog = "", additional_geogs = ""){

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
stl_merge <- function(df_original, ..., weight_var = "", method = "mean", other_grouping_vars = ""){

  weight_var <- as.character(substitute(weight_var))
  variables <- dplyr:::tbl_at_vars(df_original, vars(...))

  # If no weight variable supplied and one is needed, read in total population and join to df
  if(weight_var == "" & method == "mean"){

    weight_var <- "population"

    if("population" %not_in% names(df_original)){
      df_original %<>% left_join(population_df, by = c("FIPS", "year"))
    }
  }

  grouping_vars <- c("FIPS", "year", "sex", "race", other_grouping_vars)

  df_original %<>%
    mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")) %>%
    group_by_at(df_original %cols_in% grouping_vars)

  # For each variable to be weighted, create a new df of the applicable variables
  for(v in variables){

    df <- df_original

    if      (method == "mean") df %<>% summarise(!!v := weighted.mean(.data[[v]], .data[[weight_var]]))
    else if (method == "max")  df %<>% summarise(!!v := max(.data[[v]], na.rm = TRUE))
    else if (method == "min")  df %<>% summarise(!!v := min(.data[[v]], na.rm = TRUE))
    else if (method == "sum")  df %<>% summarise_at(v, sum)

    df %<>% ungroup()

    #add the data frame to the output
    output <- assign_col_join(output, df, by = df %cols_in% grouping_vars)

  }
  output
}

#' Joins data frames by common GLP ID variables: FIPS, MSA, year, race, and sex.
#'
#' @param ... Data frames.
#' @export
bind_df <- function(..., by = NULL){
  data_frames <- list(...)

  grouping_vars <- c("FIPS", "MSA", "zip", "tract", "neighborhood", "disctrict", "year",
                     "race", "sex", "frl_status", "demographic", "variable")

  if (is.null(by)) {
    grouping_vars <- grouping_vars[grouping_vars %in% names(data_frames[[1]])]
  }
  else {
    grouping_vars <- by
  }

  output <- purrr::reduce(data_frames, full_join, by = grouping_vars)

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


#' Organizes common GLP data by columns and rows and replaces FIPS 1073 with 01073.
#'
#' Columns: MSA, FIPS, city, year, sex, race, baseline, current,
#' Rows: MSA, FIPS, year, sex, race
#'
#' @param df A data frame
#' @export
organize <- function(df) {

  if (df_type(df) %in% c("block", "tract", "neighborhood")) {
    columns <- c("tract", "neighborhood", "block", "year", "line1", "line2", "line3")
    columns <- df %cols_in% columns
    df %<>%
      select(columns, everything()) %>%
      arrange_at(columns)

    return(df)
  }

  columns <- df %cols_in% c("MSA", "FIPS", "city", "year", "sex", "race", "frl_status", "baseline", "current")

  rows <- df %cols_in% c("MSA", "FIPS", "year", "sex", "race", "frl_status")

  df %<>%
    select(columns, everything()) %>%
    arrange_at(vars(rows))

  if("FIPS" %in% names(df)){
    df %<>%
      mutate(FIPS = replace(FIPS, FIPS == "1073", "01073"))
  }

  df
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
    "FIPS" %in% cols       ~ "FIPS",
    "MSA"  %in% cols       ~ "MSA",
    "frl_status" %in% cols ~ "ky",
    all(cols %in% c("year", "variable", "category", "value"))         ~ "graph",
    all(cols %in% c("year", "city", "variable", "category", "value")) ~ "graph_max_min",
    "block" %in% cols                                           ~ "block",
    "tract" %in% cols                                           ~ "tract",
    "neighborhood" %in% cols & "Phoenix Hill-Smoketown-Shelby Park" %in% df[["neighborhood"]] ~ "nh",
    "neighborhood" %in% cols                                    ~ "muw",
    "zip" %in% cols                                             ~ "zip",
    "market" %in% cols                                          ~ "market",
    "county" %in% cols                                          ~ "county",
    TRUE ~ NA_character_)
}

#' Check if each row of a data frame is unique
#'
#' Returns
#'
#' @param df A data frame.
#' @export
unique_check <- function(df) {
  grouping_vars <- c("MSA", "FIPS", "tract", "neighborhood",
                     "year", "sex", "race")

  grouping_vars <- df %cols_in% grouping_vars

  num_per_group <- df %>%
    group_by_at(grouping_vars) %>%
    summarise(n = n()) %>%
    arrange(n) %>%
    pull(n) %>%
    unique()

  num_per_group
}


#' Join two data frames when one might not exist
#'
#' When accumulating data within a loop, you usually have to check whether to create a data frame
#' or bind two data frames together. This function is a shortcut: it checks to see is df_1 exists.
#' If so, it binds the two data frames together as usual. If not, it catches the error and returns df_2.
#'
#' @param df_1 A data frame that might exist
#' @param df_2 A data frame to join to \code{df_1}
#' @param by   If using \code{assign_col_join}, Any values to pass to \code{full_join}.
#' If none are supplied, \code{glptools::bind_df} is used.
#' @name assign_join
NULL

#' @describeIn assign_join Joins two data frames column-wise when one might not exist
#' @export
assign_col_join <- function(df_1, df_2, by){
  tryCatch({
    if (missing(by)) bind_df(df_1, df_2)
    else             full_join(df_1, df_2, by = by)
    },
    error = function(cond){
      df_2
    })
}

#' @describeIn assign_join Joins two data frames row-wise when one might not exist
#' @export
assign_row_join <- function(df_1, df_2){
  tryCatch({
    bind_rows(df_1, df_2)
  },
  error = function(cond){
    df_2
  })
}

#' Adjust data for cost of living and inflation
#'
#'
#' @param df A data frame.
#' @param ... Variables to adjust.
#' @param remove_calc Whether to remove the columns \code{rpp_index} and \code{cpi_index} after adjustments.
#' Defaults to \code{TRUE}.
#' @export
COLA <- function(df, ..., base_year = 2018, remove_calc = TRUE, inflation = T, rpp = T){
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
    summarise(pct_na = mean(is.na(var))) %>%
    filter(pct_na < 1)

  results$year
}

#' Add or replace a file in sysdata.rda
#'   Any files in the current environment are added to the sysdata.rda file
#'
#' @export
update_sysdata <- function(...) {

  dfs_to_save <- dplyr:::dots(...) %>% unlist() %>% as.character()
  temp_env <- new.env()
  load("R/sysdata.rda", envir = temp_env)

  for (df in dfs_to_save){
    temp_env[[df]] <- get(df, envir = .GlobalEnv)
  }

  save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)
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
                        method = "percent", maps = c("tract", "nh", "muw")) {

  variables <- dplyr:::tbl_at_vars(map_df, vars(...))
  grouping_vars <- map_df %cols_in% c("year", "race", "sex")

  if (missing(pop)) {
    map_df %<>% left_join(glpdata:::population_tract, by = c("tract", "year"))
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
      group_by_at(c("tract", grouping_vars)) %>%
      mutate_at(variables, ~ fxn(., .data[[pop]])) %>%
      select(all_of(c("tract", grouping_vars, variables))) %>%
      ungroup()
  }

  if ("nh" %in% maps) {
    df_nh <- map_df %>%
      left_join(nh_tract, by = "tract") %>%
      group_by_at(c("neighborhood", grouping_vars)) %>%
      summarise_at(variables, ~ fxn(., .data[[pop]])) %>%
      ungroup()
  }

  if ("muw" %in% maps) {
    df_muw <- map_df %>%
      left_join(muw_tract, by = "tract") %>%
      group_by_at(c("neighborhood", grouping_vars)) %>%
      summarise_at(variables, ~ fxn(., .data[[pop]])) %>%
      ungroup()
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
    if ("muw" %in% maps)  df_muw    %<>% complete(nesting(year), neighborhood = "Airport")
  }

  if (pop_adjust) {
    if ("tract" %in% maps) df_tract %<>% per_capita_adj(variables)
    if ("nh" %in% maps)    df_nh    %<>% per_capita_adj(variables)
    if ("muw" %in% maps)   df_muw   %<>% per_capita_adj(variables)
  }

  # Create list to return based on map parameter
  output <-
    purrr::map(maps, ~ paste0("df_", .x) %>% assign(.,get(.))) %>%
    setNames(paste0(return_name, "_", maps))

  output
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
per_capita_adj <- function(df, ..., geog, keep_vars = T, keep_pop = F) {

  # Create list of variables from ... argument
  variables <- dplyr:::tbl_at_vars(df, vars(...))

  # Determine geography and other variables to join by
  if (missing(geog)) geog <- df %cols_in% c("MSA", "FIPS", "tract", "neighborhood", "zip")

  if(length(geog) > 1) {
    stop("Too many geography columns. Provide geog argument.")
  }

  join_vars <- c(geog, df %cols_in% c("year", "sex", "race"))

  # Create a clean, minimal population data frame
  tryCatch({
    pop_df <- switch(df_type(df),
                     "MSA"   = glpdata:::population_msa_1yr,
                     "FIPS"  = glpdata:::population_county,
                     "tract" = glpdata:::population_tract,
                     "nh"    = glpdata:::population_nh,
                     "muw"   = glpdata:::population_muw)
  },
  error = function(e){
    stop("Geography not MSA, FIPS, or tract")
  })

  if("year" %not_in% join_vars) pop_df %<>% filter(year == 2018)
  pop_df %<>% filter(sex == "total")
  pop_df %<>% filter(race == "total")

  pop_df %<>% select_at(c(join_vars, "population"))

  # Join df to population df and divide by population.
  # If keep_vars == TRUE, retain original variables.
  if (keep_vars) {
    new_df <- df %>%
      left_join(pop_df, by = join_vars) %>%
      mutate_at(variables, ~ . / population) %>%
      rename_at(variables, ~ paste0(., "_pp")) %>%
      select_at(c(join_vars, paste0(variables, "_pp"), "population"))

    df %<>% bind_df(new_df)
  } else {
    df %<>%
      left_join(pop_df, by = join_vars) %>%
      mutate_at(variables, ~ . / population)
  }

  # If keep_pop == FALSE, remove population variable
  if (!keep_pop) df %<>% select(-population)

  df
}


#' Add or replace a file in sysdata.rda
#'   Any files in the current environment are added to the sysdata.rda file
#'
#' @export
get_sysdata <- function(df) {
  temp_env <- new.env()
  load("R/sysdata.rda", envir = temp_env)

  output <- get(df, envir = temp_env)
}

#' Transform data from 2000 census tracts to 2010 census tracts
#'
#' @export
tract_00_to_10 <- function(df, years, ...) {

  id_cols <- df %cols_in% c("year", "sex", "race")

  df00 <- df %>%
    filter(year %in% years) %>%
    left_join(tract00_tract_10, by = c("tract" = "tract00")) %>%
    group_by_at(c("tract10", id_cols)) %>%
    summarise_at(vars(...), ~sum(. * percent / 100)) %>%
    ungroup() %>%
    rename(tract = tract10)

  df00 %<>% complete(nesting(!!!syms(id_cols)), tract = "21111980100")

  df10 <- df %>% filter(year %not_in% years)

  bind_rows(df00, df10) %>%
    organize()
}


