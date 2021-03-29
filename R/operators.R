#' Not in, the counterpart to %in%
#'
#' @name not_in
#' @export
`%not_in%` <- function (x, table) match(x, table, nomatch = 0L) == 0L


#' Shorthand for paste0(a, b)
#'
#' @name p
#' @export
`%p%` <- function (a, b) paste0(a, b)

#' Returns any columns in data frame \code{df} that are in the character vector \code{columns},
#'   in the order the appear in \code{columns}
#'
#' @name cols_in
#' @export
`%cols_in%` <- function (df, columns) columns[columns %in% names(df)]

#' Returns any columns in data frame \code{df} that are NOT in the character vector \code{columns},
#'   in the order the appear in \code{df}
#'
#' @name cols_not_in
#' @export
`%cols_not_in%` <- function (df, columns) names(df)[names(df) %not_in% columns]
