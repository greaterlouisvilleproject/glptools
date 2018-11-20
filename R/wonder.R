#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
wonder_time <- function(folder, geog_type){
  wd <- getwd()
  wd <- paste(wd, folder, sep = "/")
  file_names <- list.files(wd)
  file_geog <- substr(file_names, 1, 5)
  n <- length(file_names)

  for (i in 1:n){
    file_path <- paste(wd, file_names[i], sep = "/")
    data <- read_tsv(file_path)

    output <- data %>% filter(!is.na(Year))
    output[[geog_type]] <- file_geog[i]

    if(i == 1){df <- output}
    else{df <- rbind(df, output)}

  }

  df
}

#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
wonder_time_single <- function(folder, seq_var = "age", start = 0){
  wd <- getwd()
  wd <- paste(wd, folder, sep = "/")
  file_names <- list.files(wd)
  n <- length(file_names)

  for (i in 1:n){
    file_path <- paste(wd, file_names[i], sep = "/")
    data <- read_tsv(file_path)

    data[[seq_var]] <- i - 1 + start

    if(i == 1){df <- data}
    else{df <- rbind(df, data)}

  }

  df
}

#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
clean_wonder <- function(df){

  #new names for variables
  names_recode <- c(
    `County Code`           = "FIPS",
    Year                    = "year",
    `Single-Year Ages Code` = "age",
    Deaths                  = "deaths",
    Population              = "population",
    `Birth Weight 12 Code`  = "weight",
    Births                  = "births",
    `Bridged Race`          = "race",
    `Age Adjusted Rate`     = "rate",
    `Multiple Cause of death Code` = "cod")

  #list of variables to keep (same as above list)
  keep_names <- c("FIPS", "year", "age", "deaths", "population", "weight", "births", "race", "rate", "cod")

  #list of variables to change to numeric
  numeric_names <- c("year", "age", "deaths", "population", "weight", "births", "rate")

  df <- df %>%
    plyr::rename(
      replace = names_recode,
      warn_missing = FALSE)

  #convert variables to numeric and subset data frame to variables of interest
  df <- df %>%
    mutate_if(names(df) %in% numeric_names, as.numeric) %>%
    select_if(names(df) %in% keep_names)

  #filter rows containing totals (which do not contain specific years) and rename variables
  df <- df %>%
    filter(!is.na(FIPS))

  df

}

#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
stl_adj_wonder <- function(df, method = "sum", pop_var = "population"){

  #list of variables to group by for summarizing St. Louis data
  group_vars <- c("FIPS", "year", "age", "weight", "race")

  #merge St. Louis city and county
  df <- df %>%
    mutate(FIPS = replace(FIPS, FIPS == "29189" | FIPS == "29510", "MERGED"))

  #if any variables that should be weighted are present, weight the data frame
  if(method == "sum"){
    df <- df %>%
      group_by_if(names(df) %in% group_vars) %>%
      summarise_all(sum) %>%
      ungroup()
  }
  #otherwise, sum variables for the two STL counties
  else if (method == "weight"){
    df <- df %>%
      group_by_if(names(df)[names(df) %in% group_vars]) %>%
      summarise_all(rate = weighted.mean(rate, pop_var)) %>%
      ungroup()
  }
  else(df <- NA)

  df
}

#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
age_adj_rate <- function(df, var, age_var = "age", pop_var = "population"){

  df$var <- df[[var]]
  df$age_var <- df[[age_var]]
  df$pop_var <- df[[pop_var]]

  #label age groups
  age_groups <-
    data.frame(
      age_var = 0:100,
      age_group = c(
        0,
        rep(1, 4),
        rep(2, 10),
        rep(3, 10),
        rep(4, 10),
        rep(5, 10),
        rep(6, 10),
        rep(7, 10),
        rep(8, 10),
        rep(9, 10),
        rep(10, 16)))

  df <- df %>%
    left_join(age_groups, by = c("age_var"))

  #summarise variable and population variable by groups
  df <- df %>%
    group_by(age_group, FIPS, year) %>%
    summarise(
      var = sum(var, na.rm = TRUE),
      pop_var = sum(pop_var, na.rm = TRUE)) %>%
    ungroup()

  #standard population from the CDC: https://wonder.cdc.gov/wonder/help/ucd.html#2000%20Standard%20Population
  std_pop <-
    data.frame(
      age_group = 0:10,
      weight = c(
        0.013818,
        0.055317,
        0.145565,
        0.138646,
        0.135573,
        0.162613,
        0.134834,
        0.087247,
        0.066037,
        0.044842,
        0.015508))

  #subset standardized population to included ages present in the sample, standardize to total 1, and join to data frame
  std_pop <- std_pop %>%
    filter(age_group %in% unique(df$age_group)) %>%
    mutate(weight = weight / sum(weight))

  df <- df %>%
    left_join(std_pop, by = c("age_group")) %>%
    group_by(FIPS, year) %>%
    summarise(rate = sum(var / pop_var * weight) * 100000) %>%
    ungroup()

  df
}
