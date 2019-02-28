#' Not in
#' 
#' @export
`%!in%` <- function (x, table) match(x, table, nomatch = 0L) == 0L

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
`%cols_!in%` <- function (df, columns) names(df)[names(df) %!in% columns]

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
  if(mean(is.na(as.numeric(x))) < 0.9){
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
pull_peers_MSA<-function(df){
  output <- filter(df, df$MSA %in% c("24340", "41180", "36420", "46140", "24860", "28940", 
                                     "13820", "26900", "31140", "28140", "36540", "24660", 
                                     "16740", "18140", "17140", "34980", "32820", "27260", 
                                     "39580", "19380", "40060"))

  output$baseline <- 1
  output$current  <- 1

  output$baseline[output$MSA %in% c(24340, 41180, 36420, 46140, 24860, 28940)] <-0

  output$current[output$MSA %in% c(27260, 39580, 19380, 40060)] <-0
  
  output$city<-NA
  output$city[output$MSA == 24340] = "Grand Rapids"
  output$city[output$MSA == 41180] = "St. Louis"
  output$city[output$MSA == 36420] = "Oklahoma City"
  output$city[output$MSA == 46140] = "Tulsa"
  output$city[output$MSA == 24860] = "Greenville"
  output$city[output$MSA == 28940] = "Knoxville"
  output$city[output$MSA == 13820] = "Birmingham"
  output$city[output$MSA == 31140] = "Louisville"
  output$city[output$MSA == 26900] = "Indianapolis"
  output$city[output$MSA == 28140] = "Kansas City"
  output$city[output$MSA == 36540] = "Omaha"
  output$city[output$MSA == 24660] = "Greensboro"
  output$city[output$MSA == 16740] = "Charlotte"
  output$city[output$MSA == 18140] = "Columbus"
  output$city[output$MSA == 17140] = "Cincinnati"
  output$city[output$MSA == 34980] = "Nashville"
  output$city[output$MSA == 32820] = "Memphis"
  output$city[output$MSA == 27260] = "Jacksonville"
  output$city[output$MSA == 39580] = "Raleigh"
  output$city[output$MSA == 19380] = "Dayton"
  output$city[output$MSA == 40060] = "Richmond"
  
  output %<>% organize()
  
  output
}

#' Subset a data frame containing MSA data to peer cities and add current and baseline peer data.
#'
#' @param df A data frame containing the column MSA
#' @param add_info Add city names, current peer, and baseline peer columns? Defaults to TRUE.
#' @export
pull_peers_FIPS <- function(df, add_info = TRUE, county_filter = "core_counties"){
  
  if(county_filter == "core_counties"){
    df %<>% filter(FIPS %in% c(FIPS_names$FIPS, "01073", "MERGED"))
  } else if (county_filter == "MSA_counties"){
    df %<>% filter(FIPS %in% MSA_FIPS$FIPS)
  } else {
    df <- df
  }
  
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

  weight_variable <- as.character(substitute(weight_var))
  variables <- dplyr:::tbl_at_vars(df_original, vars(...))

  #If no weight variable supplied, read in total population and join to df
  if(weight_variable == ""){
    
    if("population" %!in% names(df_original)){
    pop <- population_df

      if(typeof(df_original$FIPS) == "character"){
        pop$FIPS <- as.character(pop$FIPS)
      }

    df_original %<>% left_join(pop, by = c("FIPS", "year"))
    }
    
    weight_variable <- "population"
  }

  n <- 1

  grouping_vars <- c("FIPS", "year", "sex", "race")
  #For each variable to be weighted, create a new df of the applicable variables
  for(v in 1:length(variables)){
    df  <- df_original %>% 
      select(df_original %cols_in% c(grouping_vars, 
                                     variables[n], 
                                     weight_variable))

    df$var <- df[[variables[n]]]
    df$weight_var <- df[[weight_variable]]

    df %<>%
      mutate(FIPS = replace(FIPS,
                            FIPS %in% c("29189", "29510"),
                            "MERGED")) %>%
      group_by_at(df %cols_in% grouping_vars)

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
      output <- full_join(output, df, by = df %cols_in% grouping_vars)
    }

    n = n + 1

  }
  output
}

#' Joins data frames by common GLP ID variables: FIPS, MSA, year, race, and sex.
#'
#' @param ... Data frames.
#' @export
bind_df <- function(...){
  data_frames <- list(...)

  grouping_vars <- c("FIPS", "MSA", "year", "race", "sex",
                     "district", "year", "demographic",
                     "Id")

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

  columns <- c("MSA", "FIPS", "city", "year", "sex", "race", "baseline", "current")
  columns <- columns[columns %in% names(df)]

  rows <- c("MSA", "FIPS", "year", "sex", "race")
  rows <- rows[rows %in% names(df)]

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
  if("FIPS" %in% cols){
    "county"
  } else if ("MSA" %in% cols){
    "MSA"
  } else if ("district" %in% cols){
    "ky_ed"
  } else if ("geography" %in% cols){
    "naep"
  } else if (all(cols %in% c("year", "variable", "category", "value"))){
    "graph"
  } else if (all(cols %in% c("year", "city", "variable", "category", "value"))){
    "graph_max_min"
  }
}

#' Adjust data for cost of living and inflation
#'
#' 
#' @param df A data frame.
#' @param ... Variables to adjust.
#' @param remove_calc Whether to remove the columns \code{rpp_index} and \code{cpi_index} after adjustments. 
#' Defaults to \code{TRUE}.
#' @export
COLA <- function(df, ..., remove_calc = TRUE){
  
  if(c("rpp_index") %!in% names(df)){
    df %<>% left_join(COLA_df, by = c("FIPS", "year"))
  }
  
  df %<>% mutate_at(vars(...), funs(. * rpp_index * cpi_index))
  
  if(remove_calc){
    df %<>% select(-rpp_index, -cpi_index)
  }
  
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
  
  if(class(substitute(var)) == "name"){
    var <- deparse(substitute(var))
  }
  df$var <- df[[var]]
  
  if(category == ""){
    df_subset <- df
  } else {
    if(df_type(df) == "ky_ed"){
      df_subset <- df %>% filter(demographic == category)
    } else if(df_type(df) %in% c("county", "MSA")){
      if(demographic %in% c("male", "female")){
        df_subset <- df %>% filter(sex == category)
      } else {
        df_subset <- df %>% filter(race == category)
      }
    } 
  }
  
  results <- df_subset %>%
    group_by(year) %>%
    summarise(pct_na = mean(is.na(var))) %>%
    filter(pct_na < 1)
  
  results$year
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