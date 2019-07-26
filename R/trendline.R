#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#'
trendline <- function(type, df, var,
                      cat = "", rollmean = 1,
                      plot_title = "", y_title = "",
                      caption_text = "", subtitle_text = "",
                      peers = "current", ylimits = "",
                      shading = F, pctiles = T,
                      include_hispanic = F, include_asian = T, order = "descending",
                      xmin = "", xmax = "",
                      zero_start = F, raw_code = "", label_function = NULL, axis_function = NULL){

  eval(parse(text = raw_code))

  if (length(var) == 1) df$var <- df[[var]]

  if (df_type(df) == "county" & "current" %not_in% names(df)) df %<>% pull_peers_FIPS()
  if (df_type(df) == "MSA" &    "current" %not_in% names(df)) df %<>% pull_peers_MSA()

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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
trend <- function(df, var,
                  cat = "", rollmean = 1,
                  plot_title = "", y_title = "",
                  caption_text = "", subtitle_text = "",
                  peers = "current", ylimits = "",
                  shading = F, pctiles = T,
                  include_hispanic = F, include_asian = T, order = "descending",
                  xmin = "", xmax = "", raw_code = "", label_function = NULL, axis_function = NULL){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  if (df_type(df) == "ky") type <- "kentucky" else type <- "standard"

  trendline(type, df, var,
            cat, rollmean,
            plot_title, y_title,
            caption_text, subtitle_text,
            peers, ylimits,
            shading, pctiles,
            include_hispanic, include_asian,
            order = "descending",
            xmin, xmax,
            zero_start = F, raw_code, label_function, axis_function)

}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
trend_maxmin <- function(df, var,
                         rollmean = 1,
                         plot_title = "", y_title = "",
                         caption_text = "", subtitle_text = "",
                         peers = "current", ylimits = "",
                         order = "descending",
                         xmin = "", xmax = "",
                         zero_start = F, label_function = NULL, axis_function = NULL){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  trendline("maxmin", df, var,
            cat = "", rollmean,
            plot_title, y_title,
            caption_text, subtitle_text,
            peers, ylimits,
            shading = F, pctiles = T,
            include_hispanic = F, include_asian = T,
            order,
            xmin, xmax,
            zero_start, raw_code = "", label_function, axis_function)
}


#' Returns statistics calculated for use in trendline graphs
#'
#' @param df A data frame
#' @param var A variable name
#' @param rollmean A rolling mean. Defaults to 1.
#' @param cat blank, "sex", "race", or a specific demographic grou
#' @param xmin First year
#' @param xmax Last year
#' @param peers The peer set to include
#' @export
trend_data <- function(df, var = "var", rollmean = 1, xmin = "", xmax = "",
                       cat = "", pctiles = T, peers = "current", include_hispanic = F, include_asian = T) {

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  df <- trendline("data", df, var,
                  cat, rollmean,
                  plot_title = "", y_title = "",
                  caption_text = "", subtitle_text = "",
                  peers, ylimits = "",
                  shading = F, pctiles,
                  include_hispanic, include_asian,
                  order = "",
                  xmin = xmin, xmax = xmax,
                  zero_start = F, raw_code = "", label_function = NULL, axis_function = NULL)

  if(cat == ""){
    df %<>%
      mutate(category = "total") %>%
      select(year, category, variable, value)
  }

  df %<>% rename(!!var := value)

  df
}

#' Returns statistics calculated for use in trendline graphs
#'
#' @param df A data frame
#' @param rollmean A rolling mean. Defaults to 1.
#' @param census_2000 Is the 2000 data from the census? If so, do not use rollmeanr on data from 2000. Defaults to TRUE.
#' @export
trend_data_maxmin <- function(df, var = "var", rollmean = 1, xmin = "", xmax = "",
                              order = "descending", peers = "current", zero_start = F){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  df <- trendline("data_maxmin", df, var,
                  cat = "", rollmean,
                  plot_title = "", y_title = "",
                  caption_text = "", subtitle_text = "",
                  peers, ylimits = "",
                  shading = F, pctiles = T,
                  include_hispanic = F, include_asian = T,
                  order,
                  xmin = xmin, xmax = xmax,
                  zero_start = F, raw_code = "", label_function = NULL, axis_function = NULL)

  df %<>% rename(!!var := value)

  df
}

