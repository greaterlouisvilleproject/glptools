#' GLP Trendline
#'
#' Creates a peer city trendline graph using a GLP-style data frame.
#' Columns \code{FIPS} and \code{year} are required columns.
#' Columns , \code{sex}, and \code{race} are optional columns.
#'
#' The parameters are organized into six sections:
#' \itemize{
#'   \item{\strong{Required}: \code{df}, \code{var}}
#'   \item{\strong{Filter and Adjust Data}:  \code{rollmean}, \code{xmin}, \code{xmax}, \code{peers}, \code{order}}
#'   \item{\strong{Add Text to Graph}: \code{plot_title}, \code{y_title}, \code{caption_text}, \code{subtitle_text}}
#'   \item{\strong{Graph Elements}: \code{zero_start}, \code{ylimits}, \code{pctiles}, \code{shading}}
#'   \item{\strong{Manipulate Labels}: \code{label_function}, \code{axis_function}}
#' }
#'
#' @param df A data frame
#' @param var The variable name, can be quoted or unquoted
#'
#' @param rollmean A rolling mean. Defaults to 1.
#' @param xmin First year
#' @param xmax Last year
#' @param peers \code{current}, \code{baseline}, or \code{none}.
#' @param order \code{Descending} or \code{Ascending}. (For max_min functions)
#'
#' @param cat A character vector that specifies a demographic category. Can be \code{race} or \code{sex}
#' to display a set of lines for each category, or can be a specific group like \code{white} or \code{female}.
#'
#' @param plot_title Plot title
#' @param y_title Y-axis title. Defailts to \code{Percent}.
#' @param caption_text Caption text in the bottom right of the graph.
#' @param subtitle_text Subtitle text
#'
#' @param zero_start For max_min functions: should the lines all begin at 0? Defaults to \code{FALSE}.
#' @param ylimits A vector of the form \code{c(min, max)} specifying the extent of the graph's y-axis.
#' Defaults to the minimum and maximum points on the graph with a buffer of 10\% of the points' range.
#' @param pctiles Include 25thand 75th percentile lines on graph? Defaults to \code{TRUE}.
#' @param shading Replace 25th and 75th percentile lines with shaded areas? (Currently not working.)
#'
#' @param label_function A unique label function to be used in place of the default selected based on \code{y_title}.
#' @param axis_function A unique axis label function to be used in place of the default selected based on \code{y_title}.
#' @examples
#' trend(education_county, "bach_plus")
#' @name trendline
NULL

#' @describeIn trendline Creates a peer city trendline graph using a GLP-style data frame.
#' @export
trend <- function(df, var,
                  rollmean = 1, xmin = "", xmax = "", peers = "current",
                  cat = "", plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
                  ylimits = "", pctiles = T, use_var_type = F, shading = F,
                  label_function = NULL, axis_function = NULL, year_breaks = NULL,
                  endpoint_labels = TRUE){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  if (df_type(df) == "ky") type <- "kentucky" else type <- "standard"

  tl(type, df, var,
     rollmean, xmin, xmax, peers, order = "descending",
     cat, plot_title, y_title, caption_text, subtitle_text,
     zero_start = F, ylimits, pctiles, use_var_type, shading,
     label_function, axis_function, year_breaks,
     endpoint_labels)

}

#' @describeIn trendline Creates a peer city max-min graph using a GLP-style data frame.
#' @export
trend_maxmin <- function(df, var,
                         rollmean = 1, xmin = "", xmax = "", peers = "current", order = "descending",
                         plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
                         zero_start = F, ylimits = "",  use_var_type = F,
                         label_function = NULL, axis_function = NULL, year_breaks = NULL,
                         endpoint_labels = TRUE, max_city = "", min_city = ""){


  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  tl("maxmin", df, var,
     rollmean, xmin, xmax, peers, order,
     cat = "", plot_title, y_title, caption_text, subtitle_text,
     zero_start, ylimits, pctiles = F, use_var_type, shading = F,
     label_function, axis_function, year_breaks,
     endpoint_labels, max_city, min_city)
}


#' @describeIn trendline Returns the data frame used in the trendline graph
#' @export
trend_data <- function(df, var = "var",
                       rollmean = 1, xmin = "", xmax = "", peers = "current",
                       cat = "", pctiles = T, use_var_type = F) {

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  df <- tl("data", df, var,
           rollmean, xmin, xmax, peers, order = "descending",
           cat, plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
           zero_start = F, ylimits = "", pctiles, use_var_type, shading = F,
           label_function = NULL, axis_function = NULL)

  if(cat == ""){
    df %<>%
      mutate(category = "total") %>%
      select(year, category, variable, value)
  }

  df %<>% rename(!!var := value)

  df
}

#' @describeIn trendline Returns the data frame used in the max-min graph
#' @export
trend_data_maxmin <- function(df, var = "var",
                              rollmean = 1, xmin = "", xmax = "", peers = "current", order = "descending",
                              zero_start = F, use_var_type = F, max_city = "", min_city = ""){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  df <- tl("data_maxmin", df, var,
           rollmean, xmin, xmax, peers, order,
           cat = "", plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
           zero_start, ylimits = "", pctiles = T, use_var_type, shading = F,
           label_function = NULL, axis_function = NULL, max_city = max_city, min_city = min_city)

  df %<>% rename(!!var := value)

  df
}


