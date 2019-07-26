#' Not in
#'
#' @export
`%not_in%` <- function (x, table) match(x, table, nomatch = 0L) == 0L


#' paste0(a, b)
#'
#' @export
`%p%` <- function (a, b) paste0(a, b)

#' Returns any variables in the vector
#'   that are columns in the data frame.
#'
#' @name not_in
#' @export
`%cols_in%` <- function (df, columns) names(df)[names(df) %in% columns]

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

#' Change a vector from a factor to character/numeric
#'
#' If the vector can be converted to a numeric vector and fewer than 10% of the values become NA,
#' the vector is converted to a numeric vector. Otherwise it is converted to a character vector.
#'
#' @param x A numeric or character vector encoded as a factor
#' @return A numeric or character vector
#' @export
unfactor <- function(x){
  x <- as.character(x)
  if(mean(is.na(as.numeric(x))) < 0.01){
    x <- as.numeric(x)
  }
  x
}

#' Normalize a vector
#'
#' @param x A numeric vector
#' @return A numeric vector of z-scores
#' @export
norm_z <- function(x){
  z <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

#' Subset a data frame containing MSA data to peer cities and add current and baseline peer data.
#'
#' @param df A data frame containing the column MSA
#' @export
pull_peers_MSA <- function(df, add_info = T) {

  if (add_info) {
    df %<>%
      left_join(MSA_df, by = "MSA") %>%
      filter(!is.na(city))
  } else {
    df %<>%
      filter(MSA %in% MSA_df$MSA)
  }

  df %<>% organize()

  df
}

#' Subset a data frame containing MSA data to peer cities and add current and baseline peer data.
#'
#' @param df A data frame containing the column MSA
#' @param add_info Add city names, current peer, and baseline peer columns? Defaults to TRUE.
#' @export
pull_peers_FIPS <- function(df, add_info = T, county_filter = "core_counties"){

  df %<>%
    mutate(
      FIPS = as.character(FIPS),
      FIPS = replace(FIPS, FIPS == "01073", "1073"))

  if (county_filter == "core_counties"){
    if(add_info){
      df %<>%
        left_join(FIPS_df, by = "FIPS") %>%
        filter(!is.na(city))
    } else {
      df %<>% filter(FIPS %in% FIPS_df$FIPS)
    }
  } else if (county_filter == "MSA_counties") {
    df %<>% filter(FIPS %in% MSA_FIPS$FIPS)
  }
  df %<>% organize()

  df
}

#' Adds a FIPS column to a data frame containing a column of MSA codes.
#'
#' @param df A data frame containing the column MSA
#' @export
add_FIPS_to_MSA <- function(df){

  df$FIPS<-NA
  df$FIPS[df$MSA == 24340] = 26081
  df$FIPS[df$MSA == 41180] = "MERGED"
  df$FIPS[df$MSA == 36420] = 40109
  df$FIPS[df$MSA == 46140] = 40143
  df$FIPS[df$MSA == 24860] = 45045
  df$FIPS[df$MSA == 28940] = 47093
  df$FIPS[df$MSA == 13820] = 1073
  df$FIPS[df$MSA == 31140] = 21111
  df$FIPS[df$MSA == 26900] = 18097
  df$FIPS[df$MSA == 28140] = 29095
  df$FIPS[df$MSA == 36540] = 31055
  df$FIPS[df$MSA == 24660] = 37081
  df$FIPS[df$MSA == 16740] = 37119
  df$FIPS[df$MSA == 18140] = 39049
  df$FIPS[df$MSA == 17140] = 39061
  df$FIPS[df$MSA == 34980] = 47037
  df$FIPS[df$MSA == 32820] = 47157
  df$FIPS[df$MSA == 27260] = 12031
  df$FIPS[df$MSA == 39580] = 37183
  df$FIPS[df$MSA == 19380] = 39113
  df$FIPS[df$MSA == 40060] = 51760

  df
}

#' Combines rows of data from the two St. Louis counties into one.
#'
#' @param df_original A data frame.
#' @param ... Column names to combine.
#' @param weight_var A variable to use when weighting the counties. Defaults to \code{population}.
#' @param method "mean" or "sum". Defaults to mean.
#' @export
stl_merge <- function(df_original, ..., weight_var = "", method = "mean"){

  weight_var <- as.character(substitute(weight_var))
  variables <- dplyr:::tbl_at_vars(df_original, vars(...))

  # If no weight variable supplied and one is needed, read in total population and join to df
  if(weight_var == "" & method == "mean"){

    weight_var <- "population"

    if("population" %not_in% names(df_original)){
      df_original %<>% left_join(population_df, by = c("FIPS", "year"))
    }
  }

  grouping_vars <- c("FIPS", "year", "sex", "race")

  df_original %<>%
    mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")) %>%
    group_by_at(df_original %cols_in% grouping_vars)

  # For each variable to be weighted, create a new df of the applicable variables
  for(v in variables){

    df <- df_original

    if (method == "mean") df %<>% summarise(!!v := weighted.mean(.data[[v]], .data[[weight_var]]))
    else df %<>% summarise_at(v, sum)

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
bind_df <- function(...){
  data_frames <- list(...)

  grouping_vars <- c("FIPS", "MSA", "year", "race", "sex", "frl_status",
                     "district", "year", "demographic",
                     "Id", "variable", "neighborhood")

  grouping_vars <- grouping_vars[grouping_vars %in% names(data_frames[[1]])]

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

  df %<>%

    #gather columns
    gather(-FIPS, -year, key = "variable", value = "value") %>%

    #divide columns at "."
    separate(variable, c("variable", "sex"), "\\.", extra = "drop", fill = "right") %>%

    #replace male+female columns with "total"
    mutate(sex = replace_na(sex, "total")) %>%

    #reshape data to side format
    spread(key = variable, value = value)

  df
}

#' Organizes common GLP data by columns and rows and replaces FIPS 01073 with 1073.
#'
#' Columns: MSA, FIPS, city, year, sex, race, baseline, current,
#' Rows: MSA, FIPS, year, sex, race
#'
#' @param df A data frame
#' @export
organize <- function(df) {

  if (df_type(df) %in% c("block", "tract", "neighborhood")) {
    columns <- c("GEO_ID", "neighborhood", "tract", "block", "line1", "line2", "line3")
    columns <- columns[columns %in% names(df)]
    df %<>% select(columns, everything())
    df
  }

  columns <- df %cols_in% c("MSA", "FIPS", "city", "year", "sex", "race", "frl_status", "baseline", "current")

  rows <- df %cols_in% c("MSA", "FIPS", "year", "sex", "race", "frl_status")

  df %<>%
    select(columns, everything()) %>%
    arrange_at(vars(rows))

  if("FIPS" %in% names(df)){
    df %<>%
      mutate(FIPS = replace(FIPS, FIPS == "01073", "1073"))
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

if ("FIPS" %in% cols) {
    "county"
  } else if ("MSA" %in% cols) {
    "MSA"
  } else if ("frl_status" %in% cols) {
    "ky"
  } else if (all(cols %in% c("year", "variable", "category", "value"))) {
    "graph"
  } else if (all(cols %in% c("year", "city", "variable", "category", "value"))) {
    "graph_max_min"
  } else if ("block" %in% cols) {
    "block"
  } else if ("tract" %in% cols) {
    "tract"
  } else if ("neighborhood" %in% cols | "nh" %in% cols){
    "neighborhood"
  } else if ("zip" %in% cols){
    "zip"
  } else if ("market" %in% cols){
    "market"
  } else if ("county" %in% cols){
    "county"
  } else {
    NA
  }
}

#' Join two data frames or return one
#'
#' @param df_1 A data frame that might exist
#' @param df_2 A data frame to join to \code{df_1} column-wise
#' @param by   Any values to pass to \code{full_join}.
#' If none are supplied, glptools::bind_df is used.
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

#' Join two data frames or return one
#'
#' @param df_1 A data frame that might exist
#' @param df_2 A data frame to join to \code{df_1} row-wise
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
COLA <- function(df, ..., base_year = 2017, remove_calc = TRUE, inflation = T, rpp = T){

  variables <- dplyr:::tbl_at_vars(df, vars(...))

  COLA_df %<>%
    group_by(FIPS) %>%
    mutate(base_cpi = cpi[year == base_year],
           cpi_index = base_cpi/cpi) %>%
    ungroup() %>%
    select(-cpi, -base_cpi)

  df %<>% left_join(COLA_df, by = c("FIPS", "year"))

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

# DEPRECATED

#' Calculate the 3-year rolling mean of a vector
#' Deprecated for rollmeanr
#'
#' @param x A vector
#' @return A vector of moving averages
#' @export
rollmean3 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-1],x[i],x[i+1]))
    y[1] <- NA
  }
  y
}

#' Calculate the 5-year rolling mean of a vector
#' Deprecated for rollmeanr
#'
#' @param x A vector
#' @return A vector of moving averages
#' @export
rollmean5 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-2],x[i-1],x[i],x[i+1],x[i+2]))
    y[1] <- NA
    y[2] <- NA
  }
  y
}

#' Combines rows of data from the two St. Louis counties into one. Requires variables to be in a character vector.
#' Deprecated in favor of stl_merge.
#'
#' @param df_original A data frame
#' @param variables A character vector of column names to combine
#' @param weight_var A variable to use when weighting the counties. If none is provided, population data from the ACS is used.
#' @export
weight_stl <- function(df_original, variables, weight_variable = ""){

  n <- 1

  if(weight_variable == ""){
    population_data <- read_csv("data/population_data.csv")
    population_data$FIPS <- as.numeric(population_data$FIPS)
    if(typeof(df_original$FIPS) == "character"){
      population_data$FIPS <- as.character(population_data$FIPS)
    }
    df_original <- df_original %>% left_join(population_data, by = c("FIPS", "year"))
    weight_variable <- 'population'
  }

  for(v in 1:length(variables)){
    df  <- df_original[,c('FIPS', 'year', variables[n], weight_variable)]

    df$var <- df[[variables[n]]]
    df$weight_var <- df[[weight_variable]]

    df %<>%
      mutate(FIPS = replace(FIPS, FIPS == '29189' | FIPS == '29510', 'MERGED')) %>%
      group_by(FIPS, year) %>%
      summarise(var = weighted.mean(var, weight_var)) %>%
      ungroup()

    df[[variables[n]]] <- df$var

    df %<>% select(-var)

    if(n == 1){
      output <- df
    }else{
      output <- full_join(output, df, by = c('FIPS', 'year'))
    }

    n = n + 1

  }
  output
}

#' Reshape KY Ed
reshape_ky_ed <- function(df, var_name) {
  df %<>%
    mutate(
      category = paste0(var_name, "_", category),
      category = replace(category, category == var_name %p% "_total", var_name)) %>%
    spread(key = category, value = !!var_name) %>%
    select(
      district, year, !!var_name,
      ends_with("_white"), ends_with("_black"), ends_with("_hispanic"), ends_with("_asian"),
      ends_with("_male"), ends_with("_female"), ends_with("_frl"), ends_with("nonfrl"))

  df
}
