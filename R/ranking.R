#' GLP Ranking
#'
#' Creates a peer city ranking graph using a GLP-style data frame.
#' Columns \code{FIPS} is a required column.
#' Columns \code{year}, \code{sex}, and \code{race} are optional columns.
#'
#' The parameters are organized into four sections:
#' \itemize{
#'   \item{\strong{Required}: \code{df}, \code{var}}
#'   \item{\strong{Filtering and Sorting}: \code{year}, \code{sex}, \code{race}, \code{peers}, \code{order}}
#'   \item{\strong{Add Text}: \code{plot_title}, \code{y_title}, \code{caption_text}, \code{subtitle_text}}
#'   \item{\strong{Manipulate Bar Labels}: \code{bar_label}, \code{sigfig}, \code{accuracy}, \code{label_function}, \code{alternate_text}}
#' }
#'
#' @param df A data frame
#' @param var The variable name, can be quoted or unquoted
#'
#' @param year The year of observations to use.
#' Defaults to the most recent year of data available for \code{var} in \code{df}.
#' @param sex Filters \code{df} using the sex column, if present.
#' Defaults to \code{total}, or can be \code{male} or \code{female}
#' @param race Filters \code{df} using the race column, if present.
#' Defaults to \code{total}, or can be \code{white}, \code{black}, \code{hispanic}, \code{asian}, or \code{other}.
#' @param peers \code{Current} or \code{Baseline}
#' @param order \code{Descending} or \code{Ascending}
#'
#' @param plot_title Plot title
#' @param y_title Y-axis title. Defailts to \code{Percent}.
#' @param caption_text Caption text in the bottom right of the graph..
#' @param subtitle_text Subtitle text
#'
#' @param bar_label Add labels to bars? Defaults to \code{TRUE}
#' @param sigfig Number of significant digits in bar labels.
#' @param accuracy Accuract of bar labels
#' @param label_function A unique label function to be used in place of the default selected based on \code{y_title}.
#' @param alternate_text A numeric vector that specifies which, if any, bar labels should be moved to the right side of the bar.
#' This is used to prevent text from being hidden due to small bars.
#'
#' @export
#' @examples
#' ranking(education_county, bach_plus, plot_title = "Bachelor's or Higher", caption_text = "GLP")
#'
#' ranking(jobs_county, median_earnings_gap_bw, plot_title = "Median Earnings", caption_text = "GLP",
#'         label_function = scales::dollar_format(accuracy = 1), alternate_text = 12:17)
#'
ranking <- function(df, var, plot_title = "",
                    year = NULL, sex = "total", race = "total", peers = "Current",
                    order = "Descending",
                    y_title = "Percent", caption_text = "", subtitle_text = "",
                    bar_label = TRUE, sigfig = 3, accuracy = 0.1,
                    label_function, alternate_text = NULL,
                    ranking_colors = TRUE){

  # Copy variable var to a new column for use with the '$' operator
  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))
  df$var <- df[[var]]

  # Filter to sex, race, and year
  if ("sex" %in% names(df)) df <- df[df$sex == sex,]
  if ("race" %in% names(df)) df <- df[df$race == race,]
  if("year" %in% names(df)) {
    if (is.null(year)) year <- max(years_in_df(df, var))
    df <- df[df$year %in% year,]

    if (length(year) > 1) {
      df %<>%
        group_by_at(df %cols_in% c("MSA", "FIPS")) %>%
        summarise(var = mean(var, na.rm = TRUE)) %>%
        ungroup()
    }
  }

  # Add peer data if not already present
  if (df_type(df) %in% c("FIPS", "MSA") & "current" %not_in% names(df)) df %<>% pull_peers(add_info = T)

  # Filter to peer parameter
  if (peers %in% c("current", "Current"))   df %<>% filter(current == 1)
  if (peers %in% c("baseline", "Baseline")) df %<>% filter(baseline == 1)

  # Sort according to order parameter
  if (order %in% c("descending", "Descending")) df %<>% arrange(desc(var))
  if (order %in% c("ascending", "Ascending"))   df %<>% arrange(var)

  df %<>% filter(!is.na(var))

  # Create numbered city labels for left side of graph
  df %<>%
    mutate(
      rank = row_number(),
      names = paste0(rank, ". ", city))

  # Set bar colors
  if (ranking_colors) {

    color_values <- c("#96ca4f", "#ffd600", "#db2834")
    color_names <- c("green", "yellow", "red")
    if (order %in% c("descending", "Descending")) {color_names  = rev(color_names)}

    breaks <- classInt::classIntervals(na.omit(df$var), 3, style = "jenks")
    df$color <- NA
    df$color[df$var <= breaks$brks[2]] <- color_names[1]
    df$color[df$var > breaks$brks[2] & df$var <= breaks$brks[3]] <- color_names[2]
    df$color[df$var > breaks$brks[3]] <- color_names[3]

  } else {
    df$color <- "blue"
    color_values <- "#f58021"
    color_names <- "blue"
  }

  if (order %in% c("descending", "Descending")) color_values = rev(color_values)

  # Create numeric labels
  if (!missing(label_function)) {
    label_text <- df$var %>% signif(sigfig) %>% label_function()
  } else if (y_title == "Dollars") {
    if (mean(df$var, na.rm = TRUE) > 10000) {
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = accuracy, scale = .001, suffix = "k")
    } else if (mean(df$var, na.rm = TRUE) > 100){
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = 1)
    } else {
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = .01)
    }
  } else if (stringr::str_detect(y_title, "Percent")) {
    label_text <- df$var %>% signif(sigfig) %>% scales::percent(accuracy = accuracy, scale = 1, suffix = "%")
  } else {
    label_text <- df$var %>% signif(sigfig) %>% scales::comma(accuracy = accuracy)
  }

  # Set text format, highlight and italicise Louisville text, highlight Louisville bar
  df$textcolor <- "#000000"
  df$textcolor[df$city == "Louisville"] <- "#00a9b7"

  df$linecolor <- "#ffffff"
  df$linecolor[df$city == "Louisville"] <- "#00a9b7"

  df$lou <- if_else(df$city == "Louisville", 1, 0)

  df$text_alignment <- 1.1
  if (!is.null(alternate_text)) df$text_alignment[df$rank %in% alternate_text] <- -0.1

  ### PLOT GRAPH

  # Initial plot
  p <- ggplot(data = df,
              aes(x = factor(names, levels = rev(names)),
                  y = var,
                  fill  = factor(color, levels = color_names, ordered = TRUE)))

  p <- p + guides(fill = FALSE, color = FALSE)

  # Add bars
  p <- p +
    geom_bar(aes(color = factor(lou, levels = 0:1)),
             size = 2,
             stat = "identity") +
    coord_flip() +
    ggthemes::theme_tufte()

  p <- p + scale_fill_manual(values = color_values)
  p <- p + scale_color_manual(values = c(NA_character_, "#00a9b7"))

  # Add features
  title_scale <- min(1, 48 / nchar(plot_title))

  p <- p + theme(text = element_text(family = "Verdana"),
                 plot.title = element_text(size = 50 * title_scale, hjust = 0.5, margin = margin(b = 20, unit = "pt")),
                 axis.text.y = element_text(hjust = 0,
                                            size = 40, color = rev(df$textcolor)),
                 axis.title.x = element_text(size = 40),
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(size = 10, lineheight = 0.5))

  if(subtitle_text != ""){
    p <- p + theme(plot.subtitle = element_text(hjust = 0.5, size = 30)) +
      labs(subtitle = subtitle_text)
  }

  # Add numeric labels to bars based on bar_label parameter
  if (y_title != "" & bar_label) {
    p <- p + geom_text(aes(label = label_text, hjust = text_alignment),
                       size = 15,
                       family = "Verdana")
  }

  # Add vertical line to the left side of the bars based on the h_line parameter
  if (min(df$var, na.rm = TRUE) < 0) p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 2)

  # Add remaining text
  p <- p + labs(title = plot_title, y = y_title,
                x = "", caption = caption_text)
  p
}

#' @describeIn ranking Outputs ranking data from inside the ranking function
#' @export
ranking_data <- function(df, variables, years = "", sex = "total", race = "total",
                         descending = TRUE, peers = "Current", new_vars = ""){

  # Copy variable var to a new column for use with the '$' operator
  variables <- dplyr:::tbl_at_vars(df, vars(!!enquo(variables)))

  # Add peer data if not already present
  if (df_type(df) %in% c("FIPS", "MSA") & "current" %not_in% names(df)) df %<>% pull_peers(add_info = TRUE)

  # Filter to peer parameter
  if (peers %in% c("current", "Current"))   df %<>% filter(current == 1)
  if (peers %in% c("baseline", "Baseline")) df %<>% filter(baseline == 1)

  # Filter to year, sex, and race
  if (years[1] == "") years <- max(years_in_df(df, variables[1]))
  df <- df[df$year %in% years,]
  if("sex" %in% names(df)) df <- df[df$sex == sex,]
  if("race" %in% names(df)) df <- df[df$race == race,]

  for(v in variables) {
    var_name <- v %p% "_rank"

    temp <- df %>%
      select_at(c(df_type(df), "year", tidyselect::all_of(v))) %>%
      group_by(year) %>%
      arrange_at(vars(v)) %>%
      {if (descending) arrange_at(., vars(v), ~ desc(.)) else arrange_at(., vars(v))} %>%
      mutate(!!var_name := row_number()) %>%
      ungroup()

    output <- assign_col_join(output, temp)
  }

  output %<>% organize()

  output
}
