#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
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
