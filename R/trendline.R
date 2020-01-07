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
#'   \item{\strong{Demographics}:  \code{cat}, \code{include_hispanic}, \code{include_asian}}
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
#' @param peers \code{Current} or \code{Baseline}.
#' @param order \code{Descending} or \code{Ascending}. (For max_min functions)
#'
#' @param cat A character vector that specifies a demographic category. Can be \code{race} or \code{sex}
#' to display a set of lines for each category, or can be a specific group like \code{white} or \code{female}.
#' @param include_hispanic Include lines for Hispanics, if applicable? Defaults to \code{FALSE}.
#' @param include_asian Include lines for Asians, if applicable? Defaults to \code{TRUE}. Mostly used for KY education data.
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

#' The workhorse behind other trendline functions
#'
tl <- function(type, df, var,
               rollmean = 1, xmin = "", xmax = "", peers = "current", order = "descending",
               cat = "", include_hispanic = F, include_asian = T,
               plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
               zero_start = F, ylimits = "", pctiles = T, shading = F,
               label_function = NULL, axis_function = NULL){

  if (length(var) == 1) df$var <- df[[var]]

  if (df_type(df) == "FIPS" & "current" %not_in% names(df)) df %<>% pull_peers_FIPS()
  if (df_type(df) == "MSA" &  "current" %not_in% names(df)) df %<>% pull_peers_MSA()

  # Filter data to peer set, race, sex, or other categories.
  # Create category names.
  output <- tl_filter(df, var, peers, cat, include_hispanic, include_asian)

  df        <- output[["df"]]
  cat_names <- output[["cat_names"]]

  if(xmin == "" | is.na(xmin)) xmin <- min(years_in_df(df, var))
  if(xmax == "" | is.na(xmax)) xmax <- max(years_in_df(df, var))


  # Calculate mean, percentiles, and Louisville values
  if(type %in% c("standard", "kentucky", "data")){
    df %<>% tl_reshape_data(pctiles)
  } else if(type %in% c("maxmin", "data_maxmin")) {
    df %<>% tl_reshape_data_maxmin(xmin, xmax, order, zero_start)
  }


  # Calculate rolling mean
  output <- tl_rolling_mean(df, xmin, xmax, rollmean, subtitle_text)

  df            <- output[["df"]]
  xmin          <- output[["xmin"]]
  xmax          <- output[["xmax"]]
  subtitle_text <- output[["subtitle_text"]]

  # If called from a data function, return df and exit trendline function
  if(type %in% c("data", "data_maxmin")) return(df)


  # Create line settings for the graph
  if(type %in% c("standard", "kentucky")){
    df %<>% tl_add_line_data(type, cat_names, pctiles)
  } else if(type == "maxmin") {
    df %<>% tl_add_line_data_maxmin()
  }


  # Calculate break settings
  output <- tl_break_settings(df, xmin, xmax, rollmean)

  major_break_settings <- output[["major_break_settings"]]
  minor_break_settings <- output[["minor_break_settings"]]


  # Initial plot
  g <- tl_plot(df)


  # Axis limits
  g %<>% tl_limits(df, xmin, xmax, ylimits, major_break_settings, minor_break_settings,
                   y_title, label_function, axis_function)


  # Add style
  g %<>% tl_style(plot_title, y_title, caption_text, subtitle_text, cat_names)


  #add color and line types
  if(type %in% c("standard", "kentucky")){
    g %<>% tl_lines(df, shading, cat_names, pctiles)
  } else if(type == "maxmin") {
    g %<>% tl_lines_maxmin(df)
  }

  g
}

#' @describeIn trendline Creates a peer city trendline graph using a GLP-style data frame.
#' @export
trend <- function(df, var,
                  rollmean = 1, xmin = "", xmax = "", peers = "current",
                  cat = "", include_hispanic = F, include_asian = T,
                  plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
                  ylimits = "", pctiles = T, shading = F,
                  label_function = NULL, axis_function = NULL){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  if (df_type(df) == "ky") type <- "kentucky" else type <- "standard"

  tl(type, df, var,
     rollmean, xmin, xmax, peers, order = "descending",
     cat, include_hispanic, include_asian,
     plot_title, y_title, caption_text, subtitle_text,
     zero_start = F, ylimits, pctiles, shading,
     label_function, axis_function)

}

#' @describeIn trendline Creates a peer city max-min graph using a GLP-style data frame.
#' @export
trend_maxmin <- function(df, var,
                         rollmean = 1, xmin = "", xmax = "", peers = "current", order = "descending",
                         plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
                         zero_start = F, ylimits = "",
                         label_function = NULL, axis_function = NULL){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  tl("maxmin", df, var,
     rollmean, xmin, xmax, peers, order = "descending",
     cat = "", include_hispanic = F, include_asian = T,
     plot_title, y_title, caption_text, subtitle_text,
     zero_start, ylimits, pctiles = F, shading = F,
     label_function, axis_function)
}


#' @describeIn trendline Returns the data frame used in the trendline graph
#' @export
trend_data <- function(df, var = "var",
                       rollmean = 1, xmin = "", xmax = "", peers = "current",
                       cat = "", include_hispanic = F, include_asian = T,
                       pctiles = T) {

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  df <- tl("data", df, var,
           rollmean, xmin, xmax, peers, order = "descending",
           cat, include_hispanic, include_asian,
           plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
           zero_start = F, ylimits = "", pctiles, shading = F,
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
                              zero_start = F){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  df <- tl("data_maxmin", df, var,
           rollmean, xmin, xmax, peers, order,
           cat = "", include_hispanic = F, include_asian = T,
           plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
           zero_start, ylimits = "", pctiles = T, shading = F,
           label_function = NULL, axis_function = NULL)

  df %<>% rename(!!var := value)

  df
}

