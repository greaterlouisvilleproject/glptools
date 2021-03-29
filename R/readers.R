#' GLP Readers
#'
#' Read a folder of files to a data frame
#'
#' @param folder A path to a folder containing files.
#' Years are assigned based on the file order, so the alphabetical order should match the chronological order.
#' @param starting_year The first year for which there is data.
#' @param geog The geographic level of the data, usually \code{FIPS}, \code{MSA}, or \code{tract}
#' @param additional_geogs Additional geographies to be included in the final data frame.
#' @param skip Number of lines to skip from the top of the file
#' @param col_types Types of columns to pass to the reader functions
#' @param read_fxn Reader function
#'
#' @return A data frame subset to peers
#'
#' @examples
#' acs_time("bach_plus", additional_geogs = "21067")
#'
#' @name readers
NULL

#' @describeIn readers Reads in a folder of data
#' @export
any_time <- function(folder, starting_year = 2005, skip = 0, col_types = NULL, read_fxn){
  wd <- getwd()
  directory <- paste0(wd, "/", folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, "/", folder, "/", file_names[i])

    if (missing(read_fxn)) df <- read_csv(file_path, skip = skip, col_types = col_types)
    else df <- read_fxn(file_path)

    if (!is.na(starting_year)) {
      df$year <- y
      y <- y + 1
    }

    output <- assign_row_join(output, df)
  }
  output
}

#' @describeIn readers Reads in a folder of ACS data
#' @export
acs_time <- function(folder, geog = "FIPS", starting_year = 2005, additional_geogs = ""){
  wd <- getwd()
  directory <- paste0(wd, "/", folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, "/", folder, "/", file_names[i])
    df <- read_csv(file_path, skip = 1)

    if ("Id" %in% names(df)){
      df %<>%
        rename(
          id = Id,
          `Geographic Area Name` = Geography) %>%
        select(-Id2)
    }

    if (any(str_detect(df$`Geographic Area Name`, "Metro Area"))) {
      geog_name <- "MSA"
    } else if (any(str_detect(df$`Geographic Area Name`, "Census Tract"))) {
      geog_name <- "tract"
    } else {
      geog_name <- "FIPS"
    }

    df %<>%
      rename(!!geog_name := id) %>%
      rename_all( ~ str_replace_all(., "!", "."))

    if (geog_name != "tract") {
      df %<>%
        mutate(!!geog_name := str_sub(!!sym(geog_name), 10)) %>%
        pull_peers(add_info = F, geog = geog, additional_geogs = additional_geogs)
    }

    df$year <- y
    y <- y + 1

    output <- assign_row_join(output, df)
  }
  output
}

#' @describeIn readers Reads in a folder of BRFSS data
#' @export
brfss_time <- function(folder){

  wd <- getwd()
  wd <- paste(wd, folder, sep = "/")
  file_names <- list.files(wd)
  n <- length(file_names)

  for (y in 2002:2019) {

    file_path <- paste(wd, file_names[y - 2001], sep = "/")
    df <- Hmisc::sasxport.get(file_path)

    df %<>% map_df(remove_var_label)

    age_var <-
      c("a.impage", # 2002
        rep("x.impage", 6), #2003 - 2008
        rep("age", 4), #2009 - 2012
        rep("x.age80", 7)) #2013 - 2019

    race_var <-
      c("a.racegr", # 2002
        rep("x.racegr2", 10),  #2003 - 2012
        rep("x.racegr3", 7)) #2013 - 2019

    sex_var <-
      c(rep("sex", 16), # 2002 - 2017
        "sex1", # 2018
        "x.sex") # 2019

    diabetes_var <-
      c(rep("diabetes", 2), # 2002 - 2003
        rep("diabete2", 7), # 2004 - 2010
        rep("diabete3", 8), # 2011 - 2018
        rep("diabete4", 1)) # 2019

    asthma_var <-
      c("a.asthms",
        rep("x.asthmst", 8), # 2003 - 2010
        rep("x.asthms1", 9)) # 2011 - 2019

    if (y == 2002) {

      df <- data.frame(MSA = df$a.mmsa,
                       wgt = df$a.mmsawt,
                       age = df[[age_var[y - 2001]]],
                       sex = df$sex,
                       race = df[[race_var[y - 2001]]],
                       genhlth = df$genhlth,
                       physdays = df$physhlth,
                       mentdays = df$menthlth,
                       diabetes = df$diabetes,
                       asthma   = df[[asthma_var[y - 2001]]],
                       pcp = df$persdoc2)

    } else if (y >= 2003) {

      check <- c("x.mmsa", "x.mmsawt", age_var[y - 2001], sex_var[y - 2001],
                 race_var[y - 2001], "genhlth", "physhlth", "menthlth", diabetes_var[y - 2001],
                 asthma_var[y - 2001], "persdoc2")

      if (all(check %in% names(df))) {
        print(y)
      } else {
        print(paste0(y, ": missing ", check[check %not_in% names(df)]))
      }

      df <- data.frame(MSA = df$x.mmsa,
                       wgt = df$x.mmsawt,
                       age = df[[age_var[y - 2001]]],
                       sex = df[[sex_var[y - 2001]]],
                       race = df[[race_var[y - 2001]]],
                       genhlth = df$genhlth,
                       physdays = df$physhlth,
                       mentdays = df$menthlth,
                       diabetes = df[[diabetes_var[y - 2001]]],
                       asthma   = df[[asthma_var[y - 2001]]],
                       pcp = df$persdoc2)

    }

    df$year <- y

    output <- assign_row_join(output, df)
  }

  output %<>%
    mutate(
      age = replace(age, age %in% c(7, 9), NA),
      sex = recode(sex, "male", "female", .default = NA_character_),
      race = recode(race, "white", "black", "other", "other", "hispanic", NA_character_)) %>%
    pull_peers(add_info = F) %>%
    organize()

  output
}

#' @describeIn readers Reads in a folder of County Business Pattern data
#' @export
business_time <- function(folder, starting_year = 2004){
  wd <- getwd()
  directory <- paste0(wd, folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, folder, file_names[i])
    data <- read_csv(file_path, skip=1, col_types = cols("Id2" = col_double()))

    data$year <- y

    if(y < 2008){
      data <- data %>%
        mutate(businesses = `Number of establishments`,
               employees  = as.numeric(`Number of employees`),
               payroll    = as.numeric(`Annual payroll ($1,000)`) * 100) %>%
        select(Id2, year, businesses, employees, payroll)
    } else{
      data <- data %>%
        mutate(businesses = `Number of establishments`,
               employees  = as.numeric(`Paid employees for pay period including March 12 (number)`),
               payroll    = as.numeric(`Annual payroll ($1,000)`) * 100) %>%
        select(Id2, year, businesses, employees, payroll)

    }

    y <- y + 1

    if(i == 1){
      df <- data
    }
    else{
      names(data) <- names(df)
      df <- rbind(df, data)
    }
  }
  df
}

#' @describeIn readers Reads in a folder of SAIHI data
#' @export
insurance_time <- function(directory = "", starting.year=2008){
  wd <- getwd()
  file_names <- list.files(directory)
  n<-length(file_names)
  y<-starting.year
  for (i in 1:n){
    file_path <- paste(directory, file_names[i], sep = "")
    data<-read_csv(file_path, skip=79)

    if(y > 2007){ data <- data %>% select(-X26) }

    data$statefips <- as.character(data$statefips)
    data$countyfips <- as.character(data$countyfips)

    data$statefips <- if_else( nchar(data$statefips) < 2, paste0("0",data$statefips), data$statefips)
    data$countyfips <- if_else( nchar(data$countyfips) < 3, paste0("0",data$countyfips), data$countyfips)
    data$countyfips <- if_else( nchar(data$countyfips) < 3, paste0("0",data$countyfips), data$countyfips)

    data$FIPS <- as.numeric(paste0(data$statefips, data$countyfips))
    all.peers <-subset(data, data$FIPS == 1073 |data$FIPS == 37119
                       |data$FIPS == 39061 |data$FIPS == 39049
                       |data$FIPS == 26081 |data$FIPS == 37081
                       |data$FIPS == 45045 |data$FIPS == 18097
                       |data$FIPS == 29095 |data$FIPS == 47093
                       |data$FIPS == 21111 |data$FIPS == 47157
                       |data$FIPS == 47037 |data$FIPS == 40109
                       |data$FIPS == 31055 |data$FIPS == 29189
                       |data$FIPS == 29510
                       |data$FIPS == 40143 |data$FIPS == 39113
                       |data$FIPS == 12031 |data$FIPS == 37183
                       |data$FIPS == 51760)

    all.peers$year<-y
    y<-y+1

    if(i==1){
      df<-all.peers
    }
    else{
      names(all.peers)<-names(df)
      df<-rbind(df, all.peers)
    }
  }
  df
}

#' @describeIn readers Reads in a folder of CPS unemployment data
#' @export
unemployment_time <- function(folder, starting_year = 2000){
  wd <- getwd()
  directory <- paste0(wd, folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, folder, file_names[i])
    col_names <- c("LAUS", "stateFIPS", "countyFIPS", "name", "year", "blank", "LF", "employed", "unemployed", "unemp_rate")
    data <- read_csv(file_path, skip = 6, col_names = col_names)

    data$year <- y
    y <- y + 1

    if(i == 1){
      df <- data
    }
    else{
      names(data) <- names(df)
      df <- rbind(df, data)
    }
  }
  df
}

#' @describeIn readers Read in CDC Wonder data
#'   Each file in the folder should be named according to the FIPS code of the data.
#' @export
wonder_time <- function(folder, geog_type = "FIPS"){
  wd <- getwd()
  wd <- paste(wd, folder, sep = "/")
  file_names <- list.files(wd)
  file_geog <- gsub(".txt", "", file_names)
  n <- length(file_names)

  for (i in 1:n){
    file_path <- paste(wd, file_names[i], sep = "/")
    df <- read_tsv(file_path)
    df[[geog_type]] <- file_geog[i]

    if(i == 1){output <- df}
    else{output <- rbind(output, df)}
  }

  output
}


#' Read in CDC Wonder Data where each file is an age...deprecated?
#
#' @param folder A path to a folder containing CDC Wonder data.
#' @param seq_var The variable to sequence along.
#' @param start The first year of age in the data.
#' @export
wonder_time_age <- function(folder, seq_var = "age", start = 0){
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

