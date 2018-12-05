#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
process_microdata <- function(data, gq = F){
  
  if(!gq){
    data %<>% 
      filter(GQ == 1 | GQ == 2) %>%
      select(-GQ)
  }
  
  if("MET2013" %in% names(data)){
    data %<>% 
      rename(
        year = YEAR,
        MSA = MET2013)
    
    #Label Tulsa MSA
    data$MSA[data$STATEFIP == 40 & data$year <= 2011 & data$PUMA %in% c(1200, 1100, 1000, 600)] <- 46140	
    data$MSA[data$STATEFIP == 40 & data$year >= 2012 & data$PUMA %in% c(1201, 1202, 1203, 1204, 1301, 1302, 1601)] <- 46140
  } else {
    data %<>% 
      rename(
        year = YEAR)
  }
  
  data$FIPS <- NA
  
  #PUMAs from 2005-2011
  data$FIPS[data$STATEFIP == 1 & (data$year < 2012 & data$PUMA %in% c(901, 905, 902, 903, 904))] <- 1073
  data$FIPS[data$STATEFIP == 12 & (data$year < 2012 & data$PUMA %in% c(1106, 1105, 1103, 1104, 1102, 1107))] <- 12031
  data$FIPS[data$STATEFIP == 18 & (data$year < 2012 & data$PUMA %in% c(2303, 2302, 2301, 2307, 2306, 2304, 2305))] <- 18097
  data$FIPS[data$STATEFIP == 21 & (data$year < 2012 & data$PUMA %in% c(1702, 1704, 1705, 1701, 1703))] <- 21111
  data$FIPS[data$STATEFIP == 26 & (data$year < 2012 & data$PUMA %in% c(1402, 1403, 1401, 1300))] <- 26081
  data$FIPS[data$STATEFIP == 29 & (data$year < 2012 & data$PUMA %in% c(1004, 1002, 1100, 1003, 902, 901))] <- 29095
  data$FIPS[data$STATEFIP == 29 & (data$year < 2012 & data$PUMA %in% c(1704, 1706, 1703, 1705, 1708, 1701, 1702, 1707))] <- 29189
  data$FIPS[data$STATEFIP == 29 & (data$year < 2012 & data$PUMA %in% c(1802, 1801, 1803))] <- 29510
  data$FIPS[data$STATEFIP == 31 & (data$year < 2012 & data$PUMA %in% c(903, 904, 902, 901))] <- 31055
  data$FIPS[data$STATEFIP == 37 & (data$year < 2012 & data$PUMA %in% c(1601, 1602, 1700))] <- 37081
  data$FIPS[data$STATEFIP == 37 & (data$year < 2012 & data$PUMA %in% c(901, 904, 902, 903, 905, 1000))] <- 37119
  data$FIPS[data$STATEFIP == 37 & (data$year < 2012 & data$PUMA %in% c(2701, 2703, 2702, 2602, 2601))] <- 37183
  data$FIPS[data$STATEFIP == 39 & (data$year < 2012 & data$PUMA %in% c(3107, 3101, 3103, 3109, 3106, 3108, 3102, 3104, 3105))] <- 39049
  data$FIPS[data$STATEFIP == 39 & (data$year < 2012 & data$PUMA %in% c(4503, 4502, 4401, 4404, 4501, 4403, 4402))] <- 39061
  data$FIPS[data$STATEFIP == 39 & (data$year < 2012 & data$PUMA %in% c(4102, 4101, 4103, 4000))] <- 39113
  data$FIPS[data$STATEFIP == 40 & (data$year < 2012 & data$PUMA %in% c(1400, 1302, 1301))] <- 40109
  data$FIPS[data$STATEFIP == 40 & (data$year < 2012 & data$PUMA %in% c(1200, 1100))] <- 40143
  data$FIPS[data$STATEFIP == 45 & (data$year < 2012 & data$PUMA %in% c(202, 201))] <- 45045
  data$FIPS[data$STATEFIP == 47 & (data$year < 2012 & data$PUMA %in% c(2202, 2205, 2204, 2201, 2203))] <- 47037
  data$FIPS[data$STATEFIP == 47 & (data$year < 2012 & data$PUMA %in% c(1400, 1301))] <- 47093
  data$FIPS[data$STATEFIP == 47 & (data$year < 2012 & data$PUMA %in% c(3202, 3105, 3104, 3103, 3102, 3201, 3101))] <- 47157
  data$FIPS[data$STATEFIP == 51 & (data$year < 2012 & data$PUMA %in% c(1200))] <- 51760
  
  #PUMAs from 2012 and after
  data$FIPS[data$STATEFIP == 1 & (data$year > 2011 & data$PUMA %in% c(1301, 1302, 1303, 1304, 1305))] <- 1073
  data$FIPS[data$STATEFIP == 12 & (data$year > 2011 & data$PUMA %in% c(3101, 3102, 3103, 3104, 3105, 3106, 3107))] <- 12031
  data$FIPS[data$STATEFIP == 18 & (data$year > 2011 & data$PUMA %in% c(2301, 2302, 2303, 2304, 2305, 2306, 2307))] <- 18097
  data$FIPS[data$STATEFIP == 21 & (data$year > 2011 & data$PUMA %in% c(1701, 1702, 1703, 1704, 1705, 1706))] <- 21111
  data$FIPS[data$STATEFIP == 26 & (data$year > 2011 & data$PUMA %in% c(1001, 1002, 1003, 1004))] <- 26081
  data$FIPS[data$STATEFIP == 29 & (data$year > 2011 & data$PUMA %in% c(1001, 1002, 1003, 1004, 1005))] <- 29095
  data$FIPS[data$STATEFIP == 29 & (data$year > 2011 & data$PUMA %in% c(1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808))] <- 29189
  data$FIPS[data$STATEFIP == 29 & (data$year > 2011 & data$PUMA %in% c(1901, 1902))] <- 29510
  data$FIPS[data$STATEFIP == 31 & (data$year > 2011 & data$PUMA %in% c(901, 902, 903, 904))] <- 31055
  data$FIPS[data$STATEFIP == 37 & (data$year > 2011 & data$PUMA %in% c(1701, 1702, 1703, 1704))] <- 37081
  data$FIPS[data$STATEFIP == 37 & (data$year > 2011 & data$PUMA %in% c(3101, 3102, 3103, 3104, 3105, 3106, 3107, 3108))] <- 37119
  data$FIPS[data$STATEFIP == 37 & (data$year > 2011 & data$PUMA %in% c(1201, 1202, 1203, 1204, 1205, 1206, 1207, 1208))] <- 37183
  data$FIPS[data$STATEFIP == 39 & (data$year > 2011 & data$PUMA %in% c(4101, 4102, 4103, 4104, 4105, 4106, 4107, 4108, 4109, 4110, 4111))] <- 39049
  data$FIPS[data$STATEFIP == 39 & (data$year > 2011 & data$PUMA %in% c(5501, 5502, 5503, 5504, 5505, 5506, 5507))] <- 39061
  data$FIPS[data$STATEFIP == 39 & (data$year > 2011 & data$PUMA %in% c(4601, 4602, 4603, 4604))] <- 39113
  data$FIPS[data$STATEFIP == 40 & (data$year > 2011 & data$PUMA %in% c(1001, 1002, 1003, 1004, 1005, 1006))] <- 40109
  data$FIPS[data$STATEFIP == 40 & (data$year > 2011 & data$PUMA %in% c(1201, 1202, 1203))] <- 40143
  data$FIPS[data$STATEFIP == 45 & (data$year > 2011 & data$PUMA %in% c(102, 103, 104, 105))] <- 45045
  data$FIPS[data$STATEFIP == 47 & (data$year > 2011 & data$PUMA %in% c(2501, 2502, 2503, 2504, 2505))] <- 47037
  data$FIPS[data$STATEFIP == 47 & (data$year > 2011 & data$PUMA %in% c(1602, 1603, 1604))] <- 47093
  data$FIPS[data$STATEFIP == 47 & (data$year > 2011 & data$PUMA %in% c(3201, 3202, 3203, 3204, 3205, 3206, 3207, 3208))] <- 47157
  data$FIPS[data$STATEFIP == 51 & (data$year > 2011 & data$PUMA %in% c(51235))] <- 51760
  
  data$FIPS[data$FIPS == 29189] = 'MERGED'
  data$FIPS[data$FIPS == 29510] = 'MERGED'
  
  data
}
