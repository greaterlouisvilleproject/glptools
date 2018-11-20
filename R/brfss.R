#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
brfss_time <- function(start_year = 2002){
  wd <- getwd()
  wd <- paste(wd, "brfss_download", sep = "/")
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
