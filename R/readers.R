#' Read in ACS data
#'
#' @param folder A path to a folder containing ACS data.
#' @param starting_year The first year for which there is data.
#' @export
#' @return A data frame
acs_time <- function(folder, geog = "FIPS", starting_year = 2005, additional_counties = ""){
  wd <- getwd()
  directory <- paste0(wd, "/", folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, "/", folder, "/", file_names[i])
    df <- read_csv(file_path, skip = 1)

    if ("Id" %in% names(df)) df %<>% rename(id = Id, `Geographic Area Name` = Geography) %>% select(-Id2)

    if        (any(str_detect(df$`Geographic Area Name`, "Metro Area"))) {
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
        pull_peers(add_info = F, geography = geog, additional_counties = additional_counties)
    }

    df$year <- y
    y <- y + 1

    if(i == 1){
      output <- df
    }
    else{
      names(df) <- names(output)
      output <- rbind(output, df)
    }
  }
  output
}

#' Read in a folder of CSV files
#'
#' @param folder A path to a folder containing ACS data.
#' @param starting_year The first year for which there is data.
#' @param skip Nu8mber of lines to skip.
#' @export
#' @return A data frame
any_time <- function(folder, starting_year = 2005, skip = 0, col_types = NULL){
  wd <- getwd()
  directory <- paste0(wd, "/", folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, "/", folder, "/", file_names[i])
    df <- read_csv(file_path, skip = skip, col_types = col_types)

    df$year <- y
    y <- y + 1

    output <- assign_row_join(output, df)
  }
  output
}

#' Read in BRFSS data
#'
#' @param folder A path to a folder containing BRFSS data.
#' @param starting_year The first year for which there is data.
#' @export
brfss_time <- function(folder){

  wd <- getwd()
  wd <- paste(wd, folder, sep = "/")
  file_names <- list.files(wd)
  n <- length(file_names)

  for (y in 2002:2017) {
    file_path <- paste(wd, file_names[y - 2001], sep = "/")
    df <- Hmisc::sasxport.get(file_path)

    df %<>% map_df(remove_var_label)

    age_var <-
      c("a.impage", # 2002
        "x.impage", "x.impage", "x.impage", "x.impage", "x.impage", "x.impage", #2003 - 2008
        "age", "age", "age", "age", #2009 - 2012
        "x.age80", "x.age80", "x.age80", "x.age80", "x.age80") #2013 - 2017

    race_var <-
      c("a.racegr", # 2002
        "x.racegr2", "x.racegr2", "x.racegr2", "x.racegr2", "x.racegr2",
        "x.racegr2", "x.racegr2", "x.racegr2", "x.racegr2", "x.racegr2", #2003 - 2012
        "x.racegr3", "x.racegr3", "x.racegr3", "x.racegr3", "x.racegr3") #2013 - 2017

    diabetes_var <-
      c(rep("diabetes", 2), # 2002 - 2003
        rep("diabete2", 7), # 2004 - 2010
        rep("diabete3", 7)) # 2011 - 2017

    asthma_var <-
      c("a.asthms",
        rep("x.asthmst", 8), # 2003 - 2010
        rep("x.asthms1", 7)) # 2011 - 2017

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

      df <- data.frame(MSA = df$x.mmsa,
                       wgt = df$x.mmsawt,
                       age = df[[age_var[y - 2001]]],
                       sex = df$sex,
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
    pull_peers_MSA(add_info = F) %>%
    organize()

  output
}

#' Read in County Business Pattern data
#'
#' @param folder A path to a folder containing CBP data.
#' @param starting_year The first year for which there is data.
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

#' Read in health insurance data
#'
#' @param folder A path to a folder containing SAIHI data.
#' @param starting_year The first year for which there is data.
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

#' Read in unemployment data
#'
#' @param folder A path to a folder containing CPS unemployment data.
#' @param starting_year The first year for which there is data.
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

