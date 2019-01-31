#' Process ACS microdata from IPUMS. 
#' 
#' Adds FIPS codes and the Tulsa MSA code, if appropriate.
#' Process race, sex, and education variables if present.
#' 
#' @param df A dataframe of microdata from IPUMS.
#' @param gq Include group quarters residents? Defaults to \code{FALSE}.
#' @param pull_peers Subset the data to peers? Defaults to \code{TRUE}.
#' Subsets to peer MSAs if present. Otherwise, subsets to peers counties.
#' @export
process_microdata <- function(df, gq = F, pull_peers = T){
  
  suppressWarnings(
    df %<>% 
      rename_if(names(df) %in% c("YEAR", "AGE", "SEX"), funs(str_to_lower))
  )
  
  if(!gq){
    df %<>% 
      filter(GQ == 1 | GQ == 2) %>%
      select(-GQ)
  } 
  
  if("MET2013" %in% names(df)){
    df %<>% 
      rename(MSA = MET2013)
    
    #Label Tulsa MSA
    df$MSA[df$STATEFIP == 40 & df$year <= 2011 & df$PUMA %in% c(1200, 1100, 1000, 600)] <- 46140	
    df$MSA[df$STATEFIP == 40 & df$year >= 2012 & df$PUMA %in% c(1201, 1202, 1203, 1204, 1301, 1302, 1601)] <- 46140
  }
  
  df$FIPS <- NA
  
  #PUMAs from 2005-2011
  df$FIPS[df$STATEFIP == 1 & (df$year < 2012 & df$PUMA %in% c(901, 905, 902, 903, 904))] <- 1073
  df$FIPS[df$STATEFIP == 12 & (df$year < 2012 & df$PUMA %in% c(1106, 1105, 1103, 1104, 1102, 1107))] <- 12031
  df$FIPS[df$STATEFIP == 18 & (df$year < 2012 & df$PUMA %in% c(2303, 2302, 2301, 2307, 2306, 2304, 2305))] <- 18097
  df$FIPS[df$STATEFIP == 21 & (df$year < 2012 & df$PUMA %in% c(1702, 1704, 1705, 1701, 1703))] <- 21111
  df$FIPS[df$STATEFIP == 26 & (df$year < 2012 & df$PUMA %in% c(1402, 1403, 1401, 1300))] <- 26081
  df$FIPS[df$STATEFIP == 29 & (df$year < 2012 & df$PUMA %in% c(1004, 1002, 1100, 1003, 902, 901))] <- 29095
  df$FIPS[df$STATEFIP == 29 & (df$year < 2012 & df$PUMA %in% c(1704, 1706, 1703, 1705, 1708, 1701, 1702, 1707))] <- 29189
  df$FIPS[df$STATEFIP == 29 & (df$year < 2012 & df$PUMA %in% c(1802, 1801, 1803))] <- 29510
  df$FIPS[df$STATEFIP == 31 & (df$year < 2012 & df$PUMA %in% c(903, 904, 902, 901))] <- 31055
  df$FIPS[df$STATEFIP == 37 & (df$year < 2012 & df$PUMA %in% c(1601, 1602, 1700))] <- 37081
  df$FIPS[df$STATEFIP == 37 & (df$year < 2012 & df$PUMA %in% c(901, 904, 902, 903, 905, 1000))] <- 37119
  df$FIPS[df$STATEFIP == 37 & (df$year < 2012 & df$PUMA %in% c(2701, 2703, 2702, 2602, 2601))] <- 37183
  df$FIPS[df$STATEFIP == 39 & (df$year < 2012 & df$PUMA %in% c(3107, 3101, 3103, 3109, 3106, 3108, 3102, 3104, 3105))] <- 39049
  df$FIPS[df$STATEFIP == 39 & (df$year < 2012 & df$PUMA %in% c(4503, 4502, 4401, 4404, 4501, 4403, 4402))] <- 39061
  df$FIPS[df$STATEFIP == 39 & (df$year < 2012 & df$PUMA %in% c(4102, 4101, 4103, 4000))] <- 39113
  df$FIPS[df$STATEFIP == 40 & (df$year < 2012 & df$PUMA %in% c(1400, 1302, 1301))] <- 40109
  df$FIPS[df$STATEFIP == 40 & (df$year < 2012 & df$PUMA %in% c(1200, 1100))] <- 40143
  df$FIPS[df$STATEFIP == 45 & (df$year < 2012 & df$PUMA %in% c(202, 201))] <- 45045
  df$FIPS[df$STATEFIP == 47 & (df$year < 2012 & df$PUMA %in% c(2202, 2205, 2204, 2201, 2203))] <- 47037
  df$FIPS[df$STATEFIP == 47 & (df$year < 2012 & df$PUMA %in% c(1400, 1301))] <- 47093
  df$FIPS[df$STATEFIP == 47 & (df$year < 2012 & df$PUMA %in% c(3202, 3105, 3104, 3103, 3102, 3201, 3101))] <- 47157
  df$FIPS[df$STATEFIP == 51 & (df$year < 2012 & df$PUMA %in% c(1200))] <- 51760
  
  #PUMAs from 2012 and after
  df$FIPS[df$STATEFIP == 1 & (df$year > 2011 & df$PUMA %in% c(1301, 1302, 1303, 1304, 1305))] <- 1073
  df$FIPS[df$STATEFIP == 12 & (df$year > 2011 & df$PUMA %in% c(3101, 3102, 3103, 3104, 3105, 3106, 3107))] <- 12031
  df$FIPS[df$STATEFIP == 18 & (df$year > 2011 & df$PUMA %in% c(2301, 2302, 2303, 2304, 2305, 2306, 2307))] <- 18097
  df$FIPS[df$STATEFIP == 21 & (df$year > 2011 & df$PUMA %in% c(1701, 1702, 1703, 1704, 1705, 1706))] <- 21111
  df$FIPS[df$STATEFIP == 26 & (df$year > 2011 & df$PUMA %in% c(1001, 1002, 1003, 1004))] <- 26081
  df$FIPS[df$STATEFIP == 29 & (df$year > 2011 & df$PUMA %in% c(1001, 1002, 1003, 1004, 1005))] <- 29095
  df$FIPS[df$STATEFIP == 29 & (df$year > 2011 & df$PUMA %in% c(1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808))] <- 29189
  df$FIPS[df$STATEFIP == 29 & (df$year > 2011 & df$PUMA %in% c(1901, 1902))] <- 29510
  df$FIPS[df$STATEFIP == 31 & (df$year > 2011 & df$PUMA %in% c(901, 902, 903, 904))] <- 31055
  df$FIPS[df$STATEFIP == 37 & (df$year > 2011 & df$PUMA %in% c(1701, 1702, 1703, 1704))] <- 37081
  df$FIPS[df$STATEFIP == 37 & (df$year > 2011 & df$PUMA %in% c(3101, 3102, 3103, 3104, 3105, 3106, 3107, 3108))] <- 37119
  df$FIPS[df$STATEFIP == 37 & (df$year > 2011 & df$PUMA %in% c(1201, 1202, 1203, 1204, 1205, 1206, 1207, 1208))] <- 37183
  df$FIPS[df$STATEFIP == 39 & (df$year > 2011 & df$PUMA %in% c(4101, 4102, 4103, 4104, 4105, 4106, 4107, 4108, 4109, 4110, 4111))] <- 39049
  df$FIPS[df$STATEFIP == 39 & (df$year > 2011 & df$PUMA %in% c(5501, 5502, 5503, 5504, 5505, 5506, 5507))] <- 39061
  df$FIPS[df$STATEFIP == 39 & (df$year > 2011 & df$PUMA %in% c(4601, 4602, 4603, 4604))] <- 39113
  df$FIPS[df$STATEFIP == 40 & (df$year > 2011 & df$PUMA %in% c(1001, 1002, 1003, 1004, 1005, 1006))] <- 40109
  df$FIPS[df$STATEFIP == 40 & (df$year > 2011 & df$PUMA %in% c(1201, 1202, 1203))] <- 40143
  df$FIPS[df$STATEFIP == 45 & (df$year > 2011 & df$PUMA %in% c(102, 103, 104, 105))] <- 45045
  df$FIPS[df$STATEFIP == 47 & (df$year > 2011 & df$PUMA %in% c(2501, 2502, 2503, 2504, 2505))] <- 47037
  df$FIPS[df$STATEFIP == 47 & (df$year > 2011 & df$PUMA %in% c(1602, 1603, 1604))] <- 47093
  df$FIPS[df$STATEFIP == 47 & (df$year > 2011 & df$PUMA %in% c(3201, 3202, 3203, 3204, 3205, 3206, 3207, 3208))] <- 47157
  df$FIPS[df$STATEFIP == 51 & (df$year > 2011 & df$PUMA %in% c(51235))] <- 51760
  
  df$FIPS[df$FIPS == 29189] = 'MERGED'
  df$FIPS[df$FIPS == 29510] = 'MERGED'
  
  if("MET2013" %in% names(df) & pull_peers){
    df %<>% pull_peers_MSA(add_info = FALSE)
  } else if(pull_peers){
    df %<>% pull_peers_FIPS(add_info = FALSE)
  }
  
  if("RACE" %in% names(df)){
    df$race <- "other"
    df$race[df$RACE == 1 & df$HISPAN == 0] <- "white"
    df$race[df$RACE == 2 & df$HISPAN == 0] <- "black"
    df$race[df$HISPAN == 1] <- "hispanic"
    
    df %<>% select(-RACE, -RACED, -HISPAN, -HISPAND)
  }
  
  if("sex" %in% names(df)){
    df$sex <- if_else(df$sex == 1, "male", "female")
  }
  
  if("EDUCD" %in% names(df)){
    df %<>%
      mutate(
        educ = "no_hs",
        educ = replace(educ, EDUCD %in% c(63, 64), "hs"),
        educ = replace(educ, EDUCD %in% c(65, 71), "some_col"),
        educ = replace(educ, EDUCD == 81, "assoc"),
        educ = replace(educ, EDUCD == 101, "bach"),
        educ = replace(educ, EDUCD %in% c(114, 115, 116), "grad"),
        educ = replace(educ, EDUCD == 1, NA)) %>%
      select(-EDUC, -EDUCD)
  }
  
  df %<>% select_if(names(df) %!in% 
                    c("GQ", "STATEFIP", "PUMA", "DATANUM", "CBSERIAL" ))
  
  df
}

#' Survey microdata by race and sex
#' 
#' This function uses the survey package to break down estimates by demographic group.
#'
#' @param survey A survey object containing FIPS, year, and optional race and sex columns.
#' @param var A column name to perform svymean on.
#' @param race Break down by race? Defaults to \code{TRUE}.
#' @param sex Break down by sex? Defaults to \code{TRUE}.
#' @param cross Break down by race and sex? Defaults to \code{TRUE}.
#' @export
svy_race_sex <- function(survey, var, race = T, sex = T, cross = T){
  if(class(substitute(var)) == "name"){
    var <- deparse(substitute(var))
  }
  
  form <- as.formula("~" %+% var)
  
  #Total
  results_tot <- svyby(form, ~FIPS+year,
                       design = survey, svymean, na.rm = TRUE)
  
  results <- results_tot %<>%
    select(-se) %>%
    mutate(
      sex = "total",
      race = "total")
  
  #By sex
  if(sex){
    results_sex <- svyby(form, ~FIPS+year+sex,
                         design = survey, svymean, na.rm = TRUE)
    
    results_sex %<>%
      select(-se) %>%
      mutate(race = "total")
    
    results %<>% bind_rows(results_sex)
  }
  
  #By race
  if(race){
    results_race <- svyby(form, ~FIPS+year+race,
                          design = survey, svymean, na.rm = TRUE)
    
    results_race %<>%
      select(-se) %>%
      mutate(sex = "total")
    
    results %<>% bind_rows(results_race)
  }
  
  #By race and sex
  if(cross){
    results_race_sex <- svyby(form, ~FIPS+year+sex+race,
                              design = survey, svymean, na.rm = TRUE)
    
    results_race_sex %<>%
      select(-se)
    
    results %<>% bind_rows(results_race_sex)
  }
  
  #Scale results from 0-1 to 0-100 
  results[[var]] <- results[[var]] * 100
  
  results
}