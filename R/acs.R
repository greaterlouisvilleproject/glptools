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
  
  # Rename some columns
  suppressWarnings(
    df %<>% 
      rename_at(df %cols_in% c("YEAR", "AGE", "SEX"), funs(str_to_lower))
  )
  
  # Remove group quarters residents
  if(!gq){
    df %<>% 
      filter(GQ == 1 | GQ == 2)
  } 
  
  # Rename the MSA column and label the Tulsa MSA
  if("MET2013" %in% names(df)){
    
    df %<>% rename(MSA = MET2013)
    
    df$MSA[df$STATEFIP == 40 & df$year <= 2011 & df$PUMA %in% c(1000, 1100, 1200)] <- 46140	
    df$MSA[df$STATEFIP == 40 & df$year >= 2012 & df$PUMA %in% c(1201, 1202, 1203, 1204, 1301)] <- 46140
  }
  
  # Add FIPS codes to data and rename St. Louis
  df %<>% left_join(PUMA_FIPS, by = c("STATEFIP", "PUMA", "year"))
  
  df$FIPS[df$FIPS == 29189] = 'MERGED'
  df$FIPS[df$FIPS == 29510] = 'MERGED'
  
  # Subset data to peers at the MSA or county level
  if("MET2013" %in% names(df) & pull_peers){
    df %<>% pull_peers_MSA(add_info = FALSE)
  } else if(pull_peers){
    df %<>% pull_peers_FIPS(add_info = FALSE)
  }
  
  # Recode race
  if("RACE" %in% names(df)){
    df$race <- "other"
    df$race[df$RACE == 1 & df$HISPAN == 0] <- "white"
    df$race[df$RACE == 2 & df$HISPAN == 0] <- "black"
    df$race[df$HISPAN == 1] <- "hispanic"
    
    df %<>% select(-RACE, -RACED, -HISPAN, -HISPAND)
  }
  
  # Recode sex
  if("sex" %in% names(df)){
    df$sex <- if_else(df$sex == 1, "male", "female")
  }
  
  # Recode education
  if("EDUCD" %in% names(df)){
    df %<>%
      mutate(
        educ = "no_hs",
        educ = replace(educ, EDUCD %in% c(62, 63, 64), "hs"),
        educ = replace(educ, EDUCD %in% c(65, 71), "some_col"),
        educ = replace(educ, EDUCD == 81, "assoc"),
        educ = replace(educ, EDUCD == 101, "bach"),
        educ = replace(educ, EDUCD %in% c(114, 115, 116), "grad"),
        educ = replace(educ, EDUCD == 1, NA)) %>%
      select(-EDUC, -EDUCD)
  }
  
  # Remove some variables that are no longer needed
  df %<>% 
    select(df %cols_!in% c("GQ", "STATEFIP", "PUMA", "DATANUM", "CBSERIAL" ))
  
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
  
  form <- as.formula("~" %p% var)
  
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