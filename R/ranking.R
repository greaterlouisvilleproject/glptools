#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
ranking <- function(df, var,
                    year = NULL, sex = "total", race = "total",
                    order = "Descending", peers = "Current",
                    plot_title = "", y_title = "Percent", caption_text = "",
                    text = TRUE, subtitle_text = "", sigfig = 3, accuracy = 0.1,
                    label_function = NULL, alternate_text = NULL){

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
        group_by(FIPS) %>%
        summarise(var = mean(var, na.rm = TRUE)) %>%
        ungroup()
    }
  }

  # Add peer data if not already present
  if (df_type(df) == "county" & "current" %not_in% names(df)) df %<>% pull_peers_FIPS()
  if (df_type(df) == "MSA" &    "current" %not_in% names(df)) df %<>% pull_peers_MSA()

  # Filter to peer parameter
  if (peers %in% c("current", "Current"))   df %<>% filter(current == 1)
  if (peers %in% c("baseline", "Baseline")) df %<>% filter(baseline == 1)

  # Sort according to order parameter
  if (order %in% c("descending", "Descending")) df %<>% arrange(desc(var))
  if (order %in% c("ascending", "Ascending"))   df %<>% arrange(var)

  # Create numbered city labels for left side of graph
  df %<>%
    mutate(
      rank = row_number(),
      names = paste0(rank, ". ", city))

  # Set bar colors
  breaks <- classIntervals(df$var, 3, style = "jenks")
  df$color <- NA
  df$color[df$var <= breaks$brks[2]] <- "green"
  df$color[df$var > breaks$brks[2] & df$var <= breaks$brks[3]] <- "yellow"
  df$color[df$var > breaks$brks[3]] <- "red"

  # Create numeric labels
  if (!is.null(label_function)) {
    label_text <- df$var %>% signif(sigfig) %>% label_function()
  } else if (y_title == "Dollars") {
    label_text <- df$var %>% signif(sigfig) %>% dollar(accuracy = accuracy, scale = .001, suffix = "k")
  } else if (str_detect(y_title, "Percent")) {
    label_text <- df$var %>% signif(sigfig) %>% percent(accuracy = accuracy, scale = 1, suffix = "%")
  } else {
    label_text <- df$var %>% signif(sigfig) %>% comma(accuracy = accuracy)
  }

  # Set text format, highlight and italicise Louisville text, highlight Louisville bar
  df$textfont <- "Museo Sans 300"
  df$textfont[df$city == "Louisville"] <- "Museo Sans 300 Italic"

  df$textcolor <- "black"
  df$textcolor[df$city == "Louisville"] <- "#00a9b7"

  df$linecolor <- "white"
  df$linecolor[df$city == "Louisville"] <- "#00a9b7"

  df$text_alignment <- 1.1
  if (!is.null(alternate_text)) if (18 %not_in% alternate_text) df$text_alignment[df$rank %in% alternate_text] <- -0.1

  ### PLOT GRAPH

  # Initial plot
  p <- ggplot(data = df,
              aes(x = factor(names, levels=rev(unique(names))),
                  y = var,
                  fill = factor(color)))

  p <- p + guides(fill = FALSE)

  # Add bars
  p <- p +
    geom_bar(stat="identity",
             color=rev(df$linecolor),
             size = 2) +
    coord_flip() +
    theme_tufte()

  if (order %in% c("ascending", "Ascending"))   p <- p + scale_fill_manual(values = c("#96ca4f", "#db2834", "#ffd600"))
  if (order %in% c("descending", "Descending")) p <- p + scale_fill_manual(values = c("#db2834", "#96ca4f", "#ffd600"))


  # Add features
  title_scale <- min(1, 48 / nchar(plot_title))

  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 plot.title = element_text(size = 74 * title_scale, hjust = 0.5, margin = margin(b = 20, unit = "pt")),
                 axis.text.y = element_text(hjust = 0, family = rev(df$textfont),
                                            size = 60, color = rev(df$textcolor)),
                 axis.title.x = element_text(size = 60),
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(size = 36, lineheight = 0.5))

  if(subtitle_text != ""){
    p <- p + theme(plot.subtitle = element_text(hjust = 0.5, size = 48)) +
             labs(subtitle = subtitle_text)
  }

  # Add numeric labels to bars based on text parameter
  if (y_title != "" & text) {
    p <- p + geom_text(aes(label = label_text, hjust = text_alignment),
                       size = 20,
                       family = "Museo Sans 300")
  }

  # Add vertical line to the left side of the bars based on the h_line parameter
  if (min(df$var) < 0) p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 2)

  # Add remaining text
  p <- p + labs(title = plot_title, y = y_title,
                x = "", caption = caption_text)
  p
}

#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
ranking_data <- function(df, variables, years = "", sex = "total", race = "total",
                         descending = TRUE, peers = "Current", new_vars = ""){

  # Copy variable var to a new column for use with the '$' operator
  variables <- dplyr:::tbl_at_vars(df, vars(!!enquo(variables)))

  # Add peer data if not already present
  if (df_type(df) == "county" & "current" %not_in% names(df)) df %<>% pull_peers_FIPS()
  if (df_type(df) == "MSA"    & "current" %not_in% names(df)) df %<>% pull_peers_MSA()

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
      select(FIPS, year, !!v) %>%
      group_by(year) %>%
      arrange_at(vars(v)) %>%
      {if (descending) arrange_at(., vars(v), ~ desc(.)) else arrange_at(., vars(v))} %>%
      mutate(!!var_name := row_number()) %>%
      ungroup()

    output <- assign_col_join(output, temp, by = c("FIPS", "year"))
  }

  output %<>% organize()

  output
}





#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
ranking_slope <- function (dataframe, times, measurement, grouping, title = "",
          subtitle = "", caption = "", xtextsize = 36, ytextsize = 8,
          titletextsize = 48, subtitletextsize = 10, captiontextsize = 8,
          linethickness = 1, linecolor = "ByGroup", datatextsize = 10,
          datatextfamily = "sans", datatextface = "plain", labeltextfamily = "sans",
          labeltextface = "bold") {
  my_special <- list(
    scale_x_discrete(position = "top"),
    theme_bw(),
    theme(
      panel.border = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x.top = element_text(size = xtextsize),
      axis.ticks = element_blank(),
      plot.title = element_text(size = titletextsize, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = subtitletextsize),
      plot.caption = element_text(size = captiontextsize),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 36)))

  Ndataframe <- deparse(substitute(dataframe))
  Ntimes <- deparse(substitute(times))
  Nmeasurement <- deparse(substitute(measurement))
  Ngrouping <- deparse(substitute(grouping))
  if (length(match.call()) <= 4) {
    stop("Not enough arguments passed... requires a dataframe, plus at least three variables")
  }
  argList <- as.list(match.call()[-1])
  if (!exists(Ndataframe)) {
    stop("The first object in your list '", Ndataframe, "' does not exist. It should be a dataframe",
         call. = FALSE)
  }
  if (!is(dataframe, "data.frame")) {
    stop(paste0("'", Ndataframe, "' does not appear to be a data frame"),
         call. = FALSE)
  }
  if (!Ntimes %in% names(dataframe)) {
    stop(paste0("'", Ntimes, "' is not the name of a variable in '",
                Ndataframe, "'"), call. = FALSE)
  }
  if (!Nmeasurement %in% names(dataframe)) {
    stop(paste0("'", Nmeasurement, "' is not the name of a variable in '",
                Ndataframe, "'"), call. = FALSE)
  }
  if (!deparse(substitute(grouping)) %in% names(dataframe)) {
    stop(paste0("'", Ngrouping, "' is not the name of a variable in '",
                Ndataframe, "'"), call. = FALSE)
  }
  if (!class(dataframe[[Nmeasurement]]) %in% c("integer", "numeric")) {
    stop(paste0("Sorry I need the measured variable '", Nmeasurement,
                "' to be a number"), call. = FALSE)
  }
  if (!"ordered" %in% class(dataframe[[Ntimes]])) {
    if (!"character" %in% class(dataframe[[Ntimes]])) {
      if ("factor" %in% class(dataframe[[Ntimes]])) {
        message(paste0("\nConverting '", Ntimes, "' to an ordered factor\n"))
        dataframe[[Ntimes]] <- factor(dataframe[[Ntimes]],
                                      ordered = TRUE)
      }
      else {
        stop(paste0("Sorry I need the variable '", Ntimes,
                    "' to be of class character, factor or ordered"),
             call. = FALSE)
      }
    }
  }
  times <- enquo(times)
  measurement <- enquo(measurement)
  grouping <- enquo(grouping)

  dataframe <- dataframe %>% filter(!is.na(!!times), !is.na(!!measurement), !is.na(!!grouping))

  obs_periods <- unique(levels(dataframe$year))

  dataframe$grouping <- dataframe[[Ngrouping]]
  dataframe$measurement <- dataframe[[Nmeasurement]]
  dataframe$times <- dataframe[[Ntimes]]

  all_rankings <- data.frame(
    times = factor(rep(obs_periods, each = 17)),
    measurement = rep(1:17, length(obs_periods)),
    grouping = rep("Education", 17 * length(obs_periods)),
    display = rep(FALSE,  17 * length(obs_periods)))

  dataframe$display = TRUE

  dataframe <- bind_rows(dataframe, all_rankings)



  if (length(linecolor) > 1) {
    if (length(linecolor) < length(unique(dataframe[[Ngrouping]]))) {
      message(paste0("\nYou gave me ", length(linecolor),
                     " colors I'm recycling because you have ", length(unique(dataframe[[Ngrouping]])),
                     " ", Ngrouping, "\n"))
      linecolor <- rep(linecolor, length.out = length(unique(dataframe[[Ngrouping]])))
    }
    line_geom <- list(geom_line(data = dataframe %>% filter(display == TRUE),
                                aes_(color = grouping, alpha = "display"),
                                size = linethickness),
                                scale_color_manual(values = linecolor))
  }
  else {
    if (linecolor == "ByGroup") {
      line_geom <- list(geom_line(data = dataframe %>% filter(display == TRUE),
                                  aes_(color = grouping,
                                       alpha = "display"), size = linethickness))
    }
    else {
      line_geom <- list(geom_line(data = dataframe %>% filter(display == TRUE),
                                  aes_(alpha = "display"),
                                  size = linethickness,
                                  color = linecolor))
    }
  }

  print(dataframe)

  g <- ggplot(dataframe, aes(group = grouping, y = measurement, x = times, alpha = display == TRUE)) +
    scale_y_reverse() +
    line_geom +
    scale_alpha_manual(values = c(1, 0, 1)) +
    geom_text_repel(data = dataframe %>% filter(!!times == max(!!times) & display == TRUE),
                    aes_(label = grouping), hjust = "right",
                    fontface = labeltextface, family = labeltextfamily,
                    size = ytextsize, nudge_x = 0.5, direction = "y") +
    geom_label_repel(data = dataframe %>% filter(display == FALSE),
                    aes_(x = times, y = measurement, label = measurement), size = datatextsize,
                    size = 4, force = 0, label.size = NA,
                    fontface = datatextface, family = datatextfamily,
                    inherit.aes = FALSE) +
    my_special +
    labs(title = title, subtitle = subtitle,
         caption = caption) +
    guides(alpha = "none", group = guide_legend(title="Metric")) +
    ylim(1, 17)

  g
}
