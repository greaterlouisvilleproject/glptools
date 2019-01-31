#' Read in ACS data
#' 
#' @param folder A path to a folder containing ACS data.
#' @param starting_year The first year for which there is data.
#' @export
#' @return A data frame
acs_time <- function(folder, starting_year = 2005){
  wd <- getwd()
  directory <- paste0(wd, "/", folder)
  file_names <- list.files(directory)
  n <- length(file_names)
  y <- starting_year
  for (i in 1:n){
    file_path <- paste0(wd, "/", folder, "/", file_names[i])
    data <- read_csv(file_path, skip=1, col_types = cols("Id2" = col_double()))
    names(data)[names(data) == 'Id2'] <- 'FIPS'
    all_peers <- data %>% filter(FIPS %in% c(1073, 12031, 18097, 21111, 26081,
                                             29095, 29189, 29510, 31055, 37081,
                                             37119, 37183, 39049, 39061, 39113,
                                             40109, 40143, 45045, 47037, 47093,
                                             47157, 51760))
    
    all_peers$year <- y
    y <- y + 1
    
    if(i == 1){
      df <- all_peers
    }
    else{
      names(all_peers) <- names(df)
      df <- rbind(df, all_peers)
    }
  }
  df
}

#' Read in BRFSS data
#' 
#' @param folder A path to a folder containing BRFSS data.
#' @param starting_year The first year for which there is data.
#' @export
brfss_time <- function(folder, start_year = 2002){
  wd <- getwd()
  wd <- paste(wd, folder, sep = "/")
  file_names <- list.files(wd)
  n <- length(file_names)
  y <- start_year
  
  for (i in 1:n){
    file_path <- paste(wd, file_names[i], sep = "/")
    data <- sasxport.get(file_path)
    
    data <- map_df(data, remove_var_label)
    
    if(y == 2002){
      output <- data.frame(msa = data$a.mmsa,
                           wgt = data$a.mmsawt,
                           obs = data$seqno,
                           age = data$age.mmsa,
                           hlth = data$genhlth,
                           physdays = data$physhlth,
                           mentdays = data$menthlth)
    }
    else if(y == 2003){
      output <- data.frame(msa = data$x.mmsa,
                           wgt = data$x.lmmsawt,
                           obs = data$seqno,
                           age = data$x.ageg.,
                           hlth = data$genhlth,
                           physdays = data$physhlth,
                           mentdays = data$menthlth)
    }
    else if(y >= 2004 & y <= 2010){
      output <- data.frame(msa = data$x.mmsa,
                           wgt = data$x.mmsawt,
                           obs = data$seqno,
                           age = data$age.mmsa,
                           hlth = data$genhlth,
                           physdays = data$physhlth,
                           mentdays = data$menthlth)
    }
    else if(y >= 2011){
      output <- data.frame(msa = data$x.mmsa,
                           wgt = data$x.mmsawt,
                           obs = data$seqno,
                           age = data$x.age.g,
                           hlth = data$genhlth,
                           physdays = data$physhlth,
                           mentdays = data$menthlth)
      
    }
    
    output$year <- y
    y <- y + 1
    
    if(i == 1){df <- output}
    else{df <- rbind(df, output)}
    
  }
  df
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

