#' Join two data frames when one might not exist
#'
#' When accumulating data within a loop, you usually have to check whether to create a data frame
#' or bind two data frames together. This function is a shortcut: it checks to see is df_1 exists.
#' If so, it binds the two data frames together as usual. If not, it catches the error and returns df_2.
#'
#' @param df_1 A data frame that might exist
#' @param df_2 A data frame to join to \code{df_1}
#' @param by   If using \code{assign_col_join}, Any values to pass to \code{full_join}.
#' If none are supplied, \code{glptools::bind_df} is used.
#' @name assign_join
NULL

#' @describeIn assign_join Joins two data frames column-wise when one might not exist
#' @export
assign_col_join <- function(df_1, df_2, by){
  tryCatch({
    if (missing(by)) bind_df(df_1, df_2)
    else             full_join(df_1, df_2, by = by)
  },
  error = function(cond){
    df_2
  })
}

#' @describeIn assign_join Joins two data frames row-wise when one might not exist
#' @export
assign_row_join <- function(df_1, df_2){
  tryCatch({
    bind_rows(df_1, df_2)
  },
  error = function(cond){
    df_2
  })
}

#' Calculate the rolling mean of a vector
#'
#' @param x A vector
#' @param r The number of years to average
#' @return A vector of rolling averages
#' @export
rollmeanr <- function(x, r){
  n <- length(x)
  y <- rep(NA, n)

  dif <- floor(r / 2)

  for(i in (1 + dif):(n - dif)){
    y[i] <- mean(x[(i - dif):(i + dif)])
  }

  y
}

#' Normalize a vector
#'
#' @param x A numeric vector
#' @return A numeric vector of z-scores
#' @export
norm_z <- function(x){
  z <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

#' Add or replace a file in sysdata.rda
#'   Any files in the current environment are added to the sysdata.rda file
#'
#' @export
get_sysdata <- function(df) {
  temp_env <- new.env()
  load("R/sysdata.rda", envir = temp_env)

  output <- get(df, envir = temp_env)
}

#' Add or replace a file in sysdata.rda
#'   Any files in the current environment are added to the sysdata.rda file
#'
#' @export
update_sysdata <- function(...) {

  dfs_to_save <- list(...) %>% unlist() %>% as.character()
  temp_env <- new.env()
  load("R/sysdata.rda", envir = temp_env)

  for (df in dfs_to_save){
    temp_env[[df]] <- get(df, envir = .GlobalEnv)
  }

  save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)
}

