#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
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
