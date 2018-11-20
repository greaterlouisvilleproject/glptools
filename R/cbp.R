#' County Business Patterns
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
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
