#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
rollmeanr <- function(x, r){
  n <- length(x)
  y <- rep(NA, n)

  dif <- floor(r / 2)

  for(i in (1 + dif):(n - dif)){
    y[i] <- mean(x[(i - dif):(i + dif)])
  }

  y
}


#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
rollmean3 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-1],x[i],x[i+1]))
    y[1] <- NA
  }
  y
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
#'
unfactor <- function(x){
  x <- as.character(x)
  if(mean(is.na(as.numeric(x))) < 0.9){
    x <- as.numeric(x)
  }
  x
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
trendline_data <- function(df, rollmean = 1, exclude = "", census_2000 = T) {
  df_lou <- df %>%
    filter(FIPS == 21111) %>%
    select(-FIPS, -baseline, -current, -city) %>%
    mutate(observation_type = "Louisville")

  df_q1 <- df %>%
    filter(FIPS != 21111) %>%
    select(-FIPS, -baseline, -current, -city) %>%
    group_by(year) %>%
    summarise_all(quantile, prob = 0.25, na.rm = TRUE) %>%
    mutate(observation_type = "Q1")

  df_mean <- df %>%
    filter(FIPS != 21111) %>%
    select(-FIPS, -baseline, -current, -city) %>%
    group_by(year) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate(observation_type = "mean")

  df_q3 <- df %>%
    filter(FIPS != 21111) %>%
    select(-FIPS, -baseline, -current, -city) %>%
    group_by(year) %>%
    summarise_all(quantile, prob = 0.75, na.rm = TRUE) %>%
    mutate(observation_type = "Q3")

  df <- bind_rows(df_lou, df_q1, df_mean, df_q3)

  if(census_2000){
    if(min(df$year) <= 2000){
      df_2000 <- df %>% filter(year == 2000)
      df <- df %>% filter(year > 2000)
    }

    df <- df %>%
      arrange(year) %>%
      group_by(observation_type) %>%
      mutate_at(vars(-year, -observation_type), rollmeanr, rollmean) %>%
      ungroup()

    if(exists("df_2000")){
      df <- bind_rows(df_2000, df)
    }
  } else {
    df <- df %>%
      arrange(year) %>%
      group_by(observation_type) %>%
      mutate_at(vars(-year, -observation_type), rollmeanr, rollmean) %>%
      ungroup()
  }


  df <- df %>% select(year, observation_type, everything())

  df
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
pull_peers_MSA<-function(data){
  all.peers <- filter(data, data$MSA %in% c("24340", "41180", "36420", "46140", "24860", "28940", "13820", "26900", "31140", "28140", "36540", "24660", "16740", "18140", "17140", "34980", "32820", "27260", "39580", "19380", "40060"))

  all.peers$baseline <- 1
  all.peers$current  <- 1

  all.peers$baseline[all.peers$MSA == 24340 | all.peers$MSA == 41180
                     |all.peers$MSA == 36420 | all.peers$MSA == 46140
                     |all.peers$MSA == 24860 | all.peers$MSA == 28940] <-0

  all.peers$current[all.peers$MSA == 27260 | all.peers$MSA == 39580
                    |all.peers$MSA == 19380 | all.peers$MSA == 40060] <-0
  all.peers
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
pull_peers_FIPS <- function(df, add_info = TRUE){
  df %<>% filter(FIPS %in% c(1073, 37119, 39061, 39049, 26081, 37081, 45045, 18097, 29095, 47093, 21111, 47157, 47037, 40109,
                             31055, 29189, 29510, 40143, 12031, 37183, 39113, 51760, "01073", "MERGED"))
  if(add_info == TRUE){
    df %<>%
      mutate(
        baseline = if_else(FIPS %in% c(26081, 29189, 29510, 40109, 40143, 45045, 47093, "MERGED"), 0, 1),
        current = if_else(FIPS %in% c(12031, 37183, 39113, 51760), 0, 1),
        FIPS = as.character(FIPS))

    city <- c('Grand Rapids', 'St. Louis', 'Oklahoma City', 'Tulsa', 'Greenville', 'Knoxville',
              'Birmingham', 'Indianapolis', 'Louisville', 'Kansas City', 'Omaha', 'Greensboro',
              'Charlotte', 'Columbus', 'Cincinnati', 'Nashville', 'Memphis', 'Jacksonville',
              'Raleigh', 'Dayton', 'Richmond')

    FIPS_codes <- c(26081, "MERGED", 40109, 40143, 45045, 47093, 1073, 18097, 21111, 29095, 31055,
                    37081, 37119, 39049, 39061, 47037, 47157, 12031, 37183, 39113, 51760)

    names_df <- data.frame(city, FIPS_codes, stringsAsFactors = FALSE)

    df %<>% left_join(names_df, by = c('FIPS' = 'FIPS_codes'))

    df$city[df$FIPS == "01073"] <- "Birmingham"
  }
  df
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
add_FIPS_to_MSA <- function(data){

  data$FIPS<-NA
  data$FIPS[data$MSA == 24340] = 26081
  data$FIPS[data$MSA == 41180] = "MERGED"
  data$FIPS[data$MSA == 36420] = 40109
  data$FIPS[data$MSA == 46140] = 40143
  data$FIPS[data$MSA == 24860] = 45045
  data$FIPS[data$MSA == 28940] = 47093
  data$FIPS[data$MSA == 13820] = 1073
  data$FIPS[data$MSA == 31140] = 21111
  data$FIPS[data$MSA == 26900] = 18097
  data$FIPS[data$MSA == 28140] = 29095
  data$FIPS[data$MSA == 36540] = 31055
  data$FIPS[data$MSA == 24660] = 37081
  data$FIPS[data$MSA == 16740] = 37119
  data$FIPS[data$MSA == 18140] = 39049
  data$FIPS[data$MSA == 17140] = 39061
  data$FIPS[data$MSA == 34980] = 47037
  data$FIPS[data$MSA == 32820] = 47157
  data$FIPS[data$MSA == 27260] = 12031
  data$FIPS[data$MSA == 39580] = 37183
  data$FIPS[data$MSA == 19380] = 39113
  data$FIPS[data$MSA == 40060] = 51760

  data
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
stl_merge <- function(df_original, ..., weight_var = "", method = "mean"){

  weight_variable <- as.character(substitute(weight_var))
  variables <- dplyr:::tbl_at_vars(df_original, vars(...))

  #If no weight variable supplied, read in total population and join to df
  if(weight_variable == ""){
    pop <- population_df

    if(typeof(df_original$FIPS) == "character"){
      pop$FIPS <- as.character(pop$FIPS)
    }

    df_original %<>% left_join(pop, by = c("FIPS", "year"))
    weight_variable <- "population"
  }

  n <- 1

  grouping_vars <- c("FIPS", "year", "sex", "race")
  grouping_vars <- grouping_vars[grouping_vars %in% names(df_original)]
  #For each variable to be weighted, create a new df of the applicable variables
  for(v in 1:length(variables)){
    df  <- df_original[,c(grouping_vars, variables[n], weight_variable)]

    df$var <- df[[variables[n]]]
    df$weight_var <- df[[weight_variable]]

    df %<>%
      mutate(FIPS = replace(FIPS,
                            FIPS == "29189" | FIPS == "29510",
                            "MERGED")) %>%
      group_by_if(names(df) %in% grouping_vars)

    #use a weighted mean. If method = "sum", simply add together values for STL
    if(method == "mean"){
      df %<>%
        summarise(var = weighted.mean(var, weight_var)) %>%
        ungroup()
    } else if(method == "sum"){
      df %<>%
        summarise(var = sum(var)) %>%
        ungroup()
    }

    #rename the variable to the orginal value and remove "var"
    df[[variables[n]]] <- df$var

    df %<>% select(-var)

    #add the data frame to the output
    if(n == 1){
      output <- df
    }else{
      output <- full_join(output, df, by = grouping_vars)
    }

    n = n + 1

  }
  output
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
bind_df <- function(...){
  data_frames <- list(...)

  grouping_vars <- c("FIPS", "year", "race", "sex")

  grouping_vars <- grouping_vars[grouping_vars %in% names(data_frames[[1]])]

  output <- reduce(data_frames, full_join, by = grouping_vars)

  output
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
#gather data, separate sex into column, replace NAs with total, and spread
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
`%!in%` <- function (x, table) match(x, table, nomatch = 0L) == 0L

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
`%+%` <- function(a, b) paste0(a, b)

#' Order
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
organize <- function(df) {

  columns <- c("MSA", "FIPS", "city", "year", "baseline", "current", "sex", "race")
  columns <- columns[columns %in% names(df)]

  rows <- c("MSA", "FIPS", "year", "sex", "race")
  rows <- rows[rows %in% names(df)]

  df %<>%
    select(columns, everything()) %>%
    arrange_at(vars(rows))

  df
}
