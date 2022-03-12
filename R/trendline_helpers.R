#' The workhorse behind other trendline functions
#'
tl <- function(type, df, var,
               rollmean = 1, xmin = "", xmax = "", peers = "current", order = "descending",
               cat = "", plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
               zero_start = F, ylimits = "", pctiles = T, use_var_type = F, shading = F,
               label_function = NULL, axis_function = NULL, year_breaks = NULL,
               endpoint_labels = TRUE, max_city = "", min_city = ""){

  if (length(var) == 1) df$var <- df[[var]]

  if (df_type(df) == "FIPS" & "current" %not_in% names(df)) df %<>% pull_peers(add_info = TRUE)
  if (df_type(df) == "MSA" &  "current" %not_in% names(df)) df %<>% pull_peers(add_info = TRUE)

  # Filter data to peer set, race, sex, or other categories.
  # Create category names.
  tl_filter(df, var, peers, cat, use_var_type) %>%
    list2env(envir = parent.env(environment()))

  if (xmin == "" | is.na(xmin)) xmin <- min(years_in_df(df, var))
  if (xmax == "" | is.na(xmax)) xmax <- max(years_in_df(df, var))

  # Calculate mean, percentiles, and Louisville values
  if(type %in% c("standard", "kentucky", "data")){
    df %<>% tl_reshape_data(pctiles, peers)
  } else if(type %in% c("maxmin", "data_maxmin")) {
    df %<>% tl_reshape_data_maxmin(xmin, xmax, order, zero_start, max_city, min_city)
  }

  # Calculate rolling mean
  tl_rolling_mean(df, xmin, xmax, rollmean, subtitle_text, type, zero_start) %>%
    list2env(envir = parent.env(environment()))

  # add any year breaks
  if (!is.null(year_breaks)) df %<>% tl_year_breaks(year_breaks)

  # If called from a data function, return df and exit trendline function
  if(type %in% c("data", "data_maxmin")) return(df)

  # Create line settings for the graph
  if(type %in% c("standard", "kentucky")){
    df %<>% tl_add_line_data(type, cat_names, pctiles, peers)
  } else if(type == "maxmin") {
    df %<>% tl_add_line_data_maxmin()
  }

  # Calculate break settings
  output <- tl_break_settings(df, xmin, xmax, rollmean) %>%
    list2env(envir = parent.env(environment()))

  # Initial plot
  g <- tl_plot(df)

  # Axis limits
  g %<>% tl_limits(df, xmin, xmax, ylimits, major_break_settings, minor_break_settings,
                   y_title, label_function, axis_function, endpoint_labels)

  # Add style
  g %<>% tl_style(plot_title, y_title, caption_text, subtitle_text, cat_names)

  #add color and line types
  if(type %in% c("standard", "kentucky")){
    g %<>% tl_lines(df, shading, cat_names, pctiles, peers)
  } else if(type == "maxmin") {
    g %<>% tl_lines_maxmin(df)
  }

  g
}

#' Makes a named list of objects
#'
#' The name of each object in the list matches its name in the environment.
#'
#' @param ... A list of objects
make_list <- function(...){
  dots_content <- list(...)
  dots_names <- as.character(eval(substitute(alist(...))))
  setNames(dots_content, dots_names)
}

#' Filter data to sex, race, and peer set.
#'
#' @return A data frame
tl_filter <- function(df,
                      var,
                      peers,
                      cat,
                      return_only_df = F) {
  #Filter to specified sex, race, and FRL status.
  #If sex or race are not ID variables, or if no sex or race was specified, do not subset.

  # If county or MSA, filter to peer set and non-relevant category to total
  if (df_type(df) %in% c("FIPS", "MSA") & peers %in% c("current", "Current"))   df %<>% filter(current == 1)
  if (df_type(df) %in% c("FIPS", "MSA") & peers %in% c("baseline", "Baseline")) df %<>% filter(baseline == 1)

  # Determine category names
  sexes <- c("Male" = "male", "Female" = "female")
  frls <- c("FRL" = "frl", "non-FRL" = "nonfrl")
  races <- c("White" = "white", "Black" = "black", "Hispanic" = "hispanic", "Asian" = "asian",
             "Asian/Pacific Islander" = "API", "American Indian/Alaska Native" = "AIAN",
             "Two or more races" = "two_or_more", "Race Not Listed" = "other")

  if      (length(cat) > 1)     cat_names <- cat
  else if (cat == "")           cat_names <- ""
  else if (cat == "sex")        cat_names <- sexes
  else if (cat == "race")       cat_names <- races[races %in% unique(df$race)]
  else if (cat == "frl_status") cat_names <- frls
  else                          cat_names <- cat

  # reshape multi-variable graphs
  if (length(cat) > 1) df %<>% gather(var, key = "category", value = "var")
  # reshape and rename demographic graphs
  else if (cat != "") {
    df %<>%
      filter(!!sym(cat) %in% cat_names) %>%
      rename(category = !!cat)
  }

  # filter any remaining demographic variables to totals
  if (any(names(df) %in% c("sex", "race", "frl_status"))) {
    df %<>% filter_at(df %cols_in% c("sex", "race", "frl_status"), ~ . == "total")
  }

  # Remove category if there is only one group
  if (all(cat != "") & length(cat_names) == 1) df %<>% select(-category)

  if (return_only_df) df else make_list(df, cat_names)
}

#' Reshape data to long format
#'
#' @return A data frame containing the columns year, --category--, variable, and value.
tl_reshape_data <- function(df, pctiles, peers){

  # If data frame is already formatted to graph (determined by variable column), return df asap.
  if("variable" %in% names(df)){
    df %<>%
      select(df %cols_in% c("year", "category", "variable", "var")) %>%
      rename(value = var)

    if (!pctiles) df %<>% filter(variable %not_in% c("q1", "q3"))
    return(df)
  }

  # Create a data frame with Louisville and
  #   a data frame without Louisville.
  if (df_type(df) == "FIPS"){
    df_wol <- df %>% filter(FIPS != 21111)
    lville <- df %>% filter(FIPS == 21111)
  } else if (df_type(df) == "MSA"){
    df_wol <- df %>% filter(MSA != 31140)
    lville <- df %>% filter(MSA == 31140)
  }

  #  Group the data set by year, category, if avilable.
  #  Calculate the 25th percentile, 75th percentile,
  #   and mean of the peer data set.
  df_wol %<>%
    group_by_at(df %cols_in% c("year", "category")) %>%
    summarise(q1 = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              q3 = quantile(var, prob = 0.75, na.rm = TRUE))

  # Lville data frame contains three columns
  lville %<>%
    select(lville %cols_in% c("year", "category", "var")) %>%
    rename(lou = var)

  #join 25th percentile, 75th percentile, and Louisville values
  df <- full_join(lville, df_wol, by = df %cols_in% c("year", "category"))

  #Reshape the data to long format
  df %<>% gather(lou, q1, mean, q3, key = "variable", value = "value")

  # If pctiles == FALSE, remove percentile data
  if (!pctiles) df %<>% filter(variable %not_in% c("q1", "q3"))
  if (peers == "none") df %<>% filter(variable == "lou")

  df
}

#' Reshape data to long format for trendlines displaying the best- and worst-performing peer cities.
#'
#' @return A data frame with the columns year, city, variable, category, and value
tl_reshape_data_maxmin <- function(df,
                                   xmin,
                                   xmax,
                                   order,
                                   zero_start,
                                   max_city,
                                   min_city){

  if (df_type(df) == "FIPS") {
    df %<>%
      arrange(FIPS, year) %>%
      group_by(FIPS)
  } else {
    df %<>%
      arrange(MSA, year) %>%
      group_by(MSA)
  }

  # Calculate change from xmin
  df %<>%
    filter(year %in% xmin:xmax) %>%
    mutate(change = var - first(var)) %>%
    ungroup() %>%
    select(year, city, var, change)

  # Is the "best" city the one with the largest positive
  #   growth or largest negative growth?
  if(order %in% c("descending", "Descending")){
    max_change <- "best"
    min_change <- "worst"
  } else if(order %in% c("ascending", "Ascending")){
    max_change <- "worst"
    min_change <- "best"
  }

  if (zero_start) df$var <- df$change

  # Determine if multiple cities are tied for best and worst. If so, we need to pick only one for either category.
  if (max_city == "") {
    max_cities <- df[df$change == max(df$change, na.rm = TRUE),]$city
    if (length(unique(max_cities)) > 1) {
      message("Cities are tied for max: " %p% paste(max_cities, collapse = ", ") %p% ". Specify one with max_city.")
    }
  }
  if (min_city == "") {
    min_cities <- df[df$change == min(df$change, na.rm = TRUE),]$city

    if (length(unique(min_cities)) > 1) {
      message("Cities are tied for min: " %p% paste(min_cities, collapse = ", ") %p% ". Specify one with max_city.")
    }
  }

  # Calculate which peers had the best and worst change.
  city_list <- df %>%
    filter(year == xmax) %>%
    #filter(change == max(change, na.rm = TRUE) | change == min(change, na.rm = TRUE) | city == "Louisville") %>%
    mutate(variable = case_when(

      # If a max city of min city is specified, use that first
      city == max_city ~ max_change,
      city == min_city ~ min_change,

      # Then find cities with max and min change
      (max_city == "") & change == max(change, na.rm = TRUE)  ~ max_change,
      (min_city == "") & change == min(change, na.rm = TRUE)  ~ min_change,

      # Then classify Louisville if it is not in the groups above
      city == "Louisville" ~ "Louisville",
      TRUE ~ NA_character_))  %>%
    select(city, variable) %>%
    filter(!is.na(variable))

  #calculate peer city mean
  peer_mean <- df %>%
    filter(city != "Louisville") %>%
    group_by(year) %>%
    summarise(var = mean(var, na.rm = TRUE))%>%
    mutate(
      city = "peer_mean",
      variable = "mean")

  df <- df %>%
    right_join(city_list, by = "city") %>%
    bind_rows(peer_mean) %>%
    select(year, category = city, variable, value = var)

  df
}

#' Calculate the rolling mean of a data frame
#'
#' @return A list containing df, xmin, xmax ,and subtitle_text
tl_rolling_mean <- function(df,
                            xmin,
                            xmax,
                            rollmean,
                            subtitle_text,
                            type,
                            zero_start,
                            return_only_df = F){

  # census = TRUE if 2000 is in the data frame but 2001:2004 are not.
  census <- 2000 %in% years_in_df(df, value) &
            all(2001:2004 %not_in% years_in_df(df, value))

  # If 2000 census, split the data frame into 2000
  #   and years greater than 2000
  if(census){
    df_2000 <- df %>% filter(year == 2000)
    df %<>% filter(year > 2000)
  }

  # Use rolling mean function on non-2000 data frame
  df %<>%
    arrange(year) %>%
    group_by_if(names(df) %in% c("variable", "category")) %>%
    mutate(value = rollmeanr(value, rollmean)) %>%
    ungroup()

  # If 2000 census, reassemble data frame
  if(census){
    df <- bind_rows(df_2000, df)
  }

  # If no 2000 census, increase xmin
  if(!census){
      xmin <- xmin + floor(rollmean / 2)


      # If the graph is a minmax graph starting at 0,
      # add a year where all values equal zero before the data begins.
      if (type == "maxmin" & rollmean > 1 & zero_start) {
        xmin <- xmin - 1

        df_zero <- df %>%
          filter(year == first(year)) %>%
          mutate(
            year = xmin,
            value = 0)

        print(df_zero)

        df <- df %>% bind_rows(df_zero)
      }
  }

  # Decrease xmax (regardless of 2000 census)
  xmax <- xmax - floor(rollmean / 2)

  # Filter to appropriate years based on the rolling mean
  df %<>%
    filter(year >= xmin) %>%
    filter(year <= xmax)  %>%
    filter(!is.na(value))

  # Adjust subtitle or create a new subtitle to
  #   reference the rolling mean.
  if(rollmean > 1){
    if(subtitle_text == ""){
      subtitle_text <- paste0(rollmean,"-year rolling average")
    } else {
      subtitle_text <- paste0(subtitle_text, ", ", rollmean, "-year rolling average")}
  }

  if(return_only_df){
    df
  } else {
    make_list(df, xmin, xmax, subtitle_text)
  }
}

#' Calculate the rolling mean of a data frame
#'
#' @return A list containing df, xmin, xmax ,and subtitle_text
tl_year_breaks <- function(df, year_breaks) {

  years_to_add <- crossing(
    year = year_breaks,
    variable = unique(df$variable),
    value = NA_real_)

  if ("category" %in% names(df)) years_to_add %<>% crossing(category = unique(df$category))

  df %<>%
    bind_rows(years_to_add) %>%
    organize()

  df
}

#' Add factor columns for ggplot to reference when assigning line groupings and styles
#'
#' @return A data frame
tl_add_line_data <- function(df, type, cat_names,
                             pctiles, peers){

  # line_group groups data based on which points should be connected by a line. (Each line is unique.)
  # style_group groups data based on aesthetic. (Percentile lines are identical in style.)

  # Create line_group by combining category (e.g. male, female) and variable (lou, q1, mean, q3) and factoring.
  # If no categories are included, line_group is equivalent to variable.
  # variable is initially named style_group but is recoded below.
  if ("category" %in% names(df)) {
    df$style_group <- paste0(df$category, "_", df$variable)
  } else {
    df$style_group <- df$variable
  }

  df %<>% mutate(line_group = factor(style_group))

  # Label and factor style_group with the order based on cat_names.

  # var_levels matches the values of style_group
  # var_names to use in the legend in the appropriate order
  # factor style_group using var_levels and var_names
  if (all(cat_names != "")) {
    cat_levels <- unname(cat_names) %>% paste0("_") #c("male", "female") OR c("act_english", "act_math")
    cat_labels <- names(cat_names)  %>% paste0(" ") #c("Male", "Female") OR c("English", "Math")
  } else {
    cat_levels <- ""
    cat_labels <- ""
  }

  cat_levels <- rep(cat_levels, each = 2)

  var_levels <- c(paste0(cat_levels, c("lou", "mean")),
                  paste0(cat_levels, c("q1", "q3")))

  #Text for the legend is either "Peer" or "KY" based on the data type.
  if (type == "kentucky") peer_group <- c("JCPS", "KY Mean") else peer_group <- c("Louisville", "Peer Mean")

  peer_group <- rep(peer_group, length(cat_labels))
  cat_labels  <- rep(cat_labels, each = 2)

  var_labels <- c(paste0(cat_labels, peer_group),
                  rep("25th and 75th Percentiles", length(cat_labels)))

  # If pctiles == FALSE, remove percentile portions of vectors
  if(!pctiles){
    var_levels <- var_levels[1:length(cat_labels)]
    var_labels <- var_labels[1:length(cat_labels)]
  }
  # If peers == FALSE, remove peer portions of vectors
  if(peers == "none"){
    var_levels <- var_levels[str_detect(var_levels, "lou")]
    var_labels <- var_labels[str_detect(var_labels, "Louisville")]
    var_labels <- str_remove(var_labels, " Louisville")
  }

  df$style_group <- factor(df$style_group,
                           levels = var_levels,
                           labels = var_labels,
                           ordered = TRUE)

  df
}

#' Add factor columns for ggplot to reference when assigning line groupings and styles for best and worst peer city graphs
#'
#' @return A data frame
tl_add_line_data_maxmin <- function(df){

  #line_group groups data based on which points should be connected by a line. (Each line is unique.)
  #style_group groups data based on aesthetic. (Percentile lines are identical in style.)

  #for best and worst performing peer graphs, the two are identical.
  var_levels <- c("Louisville", "best", "worst", "mean")

  var_labels <- c("Louisville",
                  paste0("Best Performer: ", unique(df$category[df$variable == "best"])),
                  paste0("Worst Performer: ", unique(df$category[df$variable == "worst"])),
                  "Peer Mean")

  if(length(unique(df$variable)) == 3){
    var_levels <- var_levels[2:4]
    var_labels <- var_labels[2:4]
  }

  df$line_group <- factor(df$variable,
                          levels = var_levels,
                          labels = var_labels,
                          ordered = TRUE)

  df$style_group <- df$line_group

  df
}

#' Calculate x-axis break settings
#'
#' Major break settings describe where the x-axis labels fall.
#' Minor break settings describe where all other vertical lines fall.
#'
#' @param ... df, xmin, xmax, rollmean
#' @return major break settings and minor break settings
tl_break_settings <- function(df, xmin, xmax, rollmean){

  # If 5 or fewer years of data displayed, show every year on x-axis
  # Otherwise, show every other year on x-axis
  if(xmax - xmin <= 5){
    skip = 1
  } else {
    skip = 2
  }

  # census = TRUE if 2000 is in the data frame bu 2001:2004 are not.
  census <- 2000 %in% years_in_df(df) &
    all(2001:2004 %not_in% years_in_df(df))

  # If census, then skip interim years not in the data.
  first_value <- if_else(census, 2005 + floor(rollmean / 2), xmin)

  # The major break settings are generated backwards to ensure the most
  # recent year is labeled and receives a major break line regardless
  # of whether there are an even or odd number of years. (It's reversed later.)
  major_break_settings <- seq(xmax, first_value, skip * -1)

  # Minor breaks are generated unless there are not enough years for skip == 2.
  # They are also generated in reverse to align correctly with major breaks.
  if (skip == 2) {
    minor_break_settings <- seq(xmax - 1, first_value, skip * -1)
  } else {
    minor_break_settings = waiver()
  }

  # If the data includes 2000, 2000 is added to the major breaks.
  if (census) {
    major_break_settings = c(major_break_settings, 2000)
  }

  # Reverse the break settings
  major_break_settings = rev(major_break_settings)
  minor_break_settings = rev(minor_break_settings)

  make_list(major_break_settings, minor_break_settings)
}

#' Initial plot
#'
#' @param df A data frame
#' @return A ggplot object
tl_plot <- function(df){
  p <- ggplot(data = df,
              aes(x = year, y = value,
                  group = line_group,
                  colour = style_group,
                  linetype = style_group,
                  alpha = style_group,
                  label = value))
  p <- p +
    geom_point(size = 4) +
    geom_line(size = 2)

  if (min(df$value, na.rm = T) < 0) p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 0.75)

  p
}

#' Add x and y axis limits and plot labels
#'
#' @return A ggplot object
tl_limits <- function(p, df, xmin, xmax, ylimits,
                      major_break_settings, minor_break_settings, y_title,
                      label_function, axis_function, endpoint_labels){

  if(length(ylimits) == 1){
    #midpoint <- (max(df$value, na.rm = TRUE) +
    #             min(df$value, na.rm = TRUE))/2
    #
    #border_space <- abs(.1 * midpoint)

    border_space = (max(df$value, na.rm = TRUE) - min(df$value, na.rm = TRUE)) * 0.1

    ylimits <- c(min(df$value, na.rm = TRUE) - border_space,
                 max(df$value, na.rm = TRUE) + border_space)
  }

  y_axis_height <- ylimits[2] - ylimits[1]

  # Create data endpoint labels
  df_label <- df %>% filter(year == xmax)

  if(!is.null(label_function)) {
    label_text <- df_label$value %>% label_function()
    axis_format <- axis_function
  } else  if (y_title == "Dollars") {
    label_text <- df_label$value %>% dollar(accuracy = 0.1, scale = .001, suffix = "k")
    axis_format <-            dollar_format(accuracy = 1,   scale = .001, suffix = "k")
  } else if (y_title == "Percent") {
    label_text <- df_label$value %>% percent(accuracy = 0.1, scale = 1, suffix = "%")
    axis_format <-            percent_format(accuracy = 1,   scale = 1, suffix = "%")
  } else if (y_title == "Score" & y_axis_height < 20) {
    label_text <- df_label$value %>% comma(accuracy = 0.1)
    axis_format <-            comma_format()
  } else if (y_title == "Score") {
    label_text <- df_label$value %>% comma()
    axis_format <-            comma_format()
  } else if (y_axis_height > 1000){
    label_text <- df_label$value %>% comma(accuracy = 100)
    axis_format <-            comma_format()
  } else if (y_axis_height < 2.5){
    label_text <- df_label$value %>% comma(accuracy = 0.1)
    axis_format <-            comma_format(accuracy = 0.5)
  } else {
    label_text <- df_label$value %>% comma(accuracy = 0.1)
    axis_format <-            comma_format(accuracy = 1)
  }

  label_length <- max(nchar(label_text))

  if (endpoint_labels) {
    xmax_adjustment <- 0.01 + 0.0125 * label_length
  } else (
    xmax_adjustment <- 0
  )

  p <- p +
    scale_x_continuous(
      limits = c(xmin, xmax + (xmax - xmin) * xmax_adjustment),
      breaks = major_break_settings,
      minor_breaks = minor_break_settings) +
    scale_y_continuous(
      limits = ylimits,
      breaks = pretty_breaks(),
      labels = axis_format)

  if (endpoint_labels) {
    p <- p +
      geom_text_repel(
        data = df_label,
        aes(label = label_text),
        xlim = c(xmax + (xmax - xmin) * .01, xmax + (xmax - xmin) * xmax_adjustment),
        size = 20,
        hjust = 0,
        alpha = 1,
        segment.alpha = 0,
        family = "Museo Sans",
        show.legend = FALSE)
  }

  p
}

#' Add style elements
#'
#' @return A ggplot object
tl_style <- function(p, plot_title, y_title,
                     caption_text, subtitle_text,
                     cat_names, x_title = "Year"){

  title_scale <- min(1, 25 / nchar(plot_title))

  #adjust theme
  p <- p + theme_bw(
    base_size = 11,
    base_family = "Museo Sans")

  p <- p + theme(
    legend.title     = element_blank(),
    legend.position  = "top",
    legend.margin    = margin(t = 10, unit = "pt"),
    legend.spacing.x = unit(30, "pt"),
    legend.text      = element_text(size = 40,
                                    margin = margin(b = 15, t = 15, unit = "pt")),

    axis.text    = element_text(size = 50),
    axis.title   = element_text(size = 60),
    axis.title.x = element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y = element_text(margin = margin(r = 15, unit = "pt")),

    plot.title = element_text(size = 90 * title_scale,
                              hjust = .5,
                              margin = margin(b = 25, unit = "pt")),

    plot.caption = element_text(size = 40,
                                lineheight = 0.5))


  #add labels
  p <- p + labs(
    title   = plot_title,
    x       = x_title,
    y       = y_title,
    caption = caption_text)

  #add subtitle if included
  if(subtitle_text != ""){
    p <- p +
      theme(plot.subtitle = element_text(hjust = 0.5, size = 50)) +
      labs(subtitle = subtitle_text)
  }

  # If two rows of legend entries will be displayed, align the categories vertically.
  if(length(cat_names) >= 4){
    p <- p + guides(colour = guide_legend(label.position = "top",
                                          keywidth = unit(12, "lines")))
  } else {
    p <- p + guides(colour = guide_legend(label.position = "top"))
  }

  p <- p +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
      legend.key = element_rect(fill = "transparent",colour = NA)
    )

  p
}

#' Add lines
#'
#' @return A ggplot object
tl_lines <- function(p, df, shading, cat_names, pctiles, peers){

  #Extract stlyle labels to match setting with legend
  line_types <- levels(df$style_group)

  #caluculate number of categories
  v <- (length(cat_names))

  #set line palette
  if(v == 1){
    line_col   <- c("#00a9b7", "black", "grey50")
  } else {
    pal <- c("#0e4a99", "#f58021", "#00a9b7", "#800055", "#356E39", "#CFB94C", "#7E9C80")[1:v]
    if (peers != "none") line_col   <- c(rep(pal, each = 2))
    if (pctiles) line_col = c(line_col, "grey50")
  }

  #set line type
  if (peers != "none") {
    line_type  <- rep(c("solid", "dashed"), v)
    line_alpha  <- rep(c(1, 0.7), v)
  } else {
    line_type <- rep("solid", v)
    line_alpha  <- rep(1, v)
  }

  #set line alphas. If shading is used, remove percentile lines
  if (pctiles) {
    line_type = c(line_type, "dashed")
    line_alpha = c(line_alpha, 0.7)
  } else if (shading){
    line_alpha = c(line_alpha, 0)
  }

  p <- p +
    scale_colour_manual(
      values = line_col,
      labels = line_types) +
    scale_linetype_manual(
      values = line_type,
      labels = line_types) +
    scale_alpha_manual(
      values = line_alpha,
      labels = line_types)

  if(shading){
    df$line_group <- as.character(df$line_group)

    positions <- data.frame(
      variable = df$style_group,
      grouping = str_split(df$line_group, "_", simplify = T)[,1],
      quartile = str_split(df$line_group, "_", simplify = T)[,2],
      year     = df$year,
      value    = df$value)

    positions %<>%
      filter(variable == "25th and 75th Percentiles")

    q1 <- positions %>%
      filter(quartile == "q1") %>%
      arrange(grouping, year)

    q3 <- positions %>%
      filter(quartile == "q3") %>%
      arrange(grouping, desc(year))

    positions <- bind_rows(q1, q3)

    positions$grouping <- factor(positions$grouping,
                                 levels = str_to_lower(cat_names),
                                 labels = cat_names)

    p <- p + geom_polygon(data = positions,
                          inherit.aes = FALSE,
                          aes(x = year, y = value,
                              group = grouping,
                              fill = factor(grouping),
                              color = factor(grouping)),
                          col = NA, alpha = 0.3) +
      scale_fill_manual(values = line_col)
    }

  p
}

#' BRFSS
#'
tl_lines_maxmin <- function(p, df){
  #Extract stlyle labels to match setting with legend

  line_types <- levels(df$style_group)

  #caluculate number of categories
  v <- length(line_types)

  # Set line palette using colors from the regular trendline pallete

  line_col <- c("#00a9b7", "#4DAF4A", "#E41A1C", "grey50")
  if(v == 3){
    line_col <- line_col[2:4]
  }

  #set line type
  line_type  <- rep("solid", v)

  #set line alphas. If shading is used, remove percentile lines
  line_alpha <- rep(1, v)

  p <- p +
    scale_colour_manual(
      values = line_col,
      labels = line_types) +
    scale_linetype_manual(
      values = line_type,
      labels = line_types) +
    scale_alpha_manual(
      values = line_alpha,
      labels = line_types)

  p
}
