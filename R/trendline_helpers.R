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
                      peers, 
                      cat,
                      include_hispanic,
                      return_only_df = F) {
  
  #Filter to specified sex and race.
  #If sex or race are not ID variables, or if no sex or race was specified, do not subset.
  
  #Filter to peer set.
  if(df_type(df) %in% c("county", "MSA")){
    if(peers %in% c("current", "Current")){
      df %<>% filter(current == 1)
    } else if(peers %in% c("baseline", "Baseline")){
      df %<>% filter(baseline == 1)
    }
  }
  
  races <- c("white", "black", "hispanic", "asian")
  sexes <- c("male", "female")
    
  # Rename category column
  if(df_type(df) %in% c("ky_ed", "naep")){
    df %<>% rename(category = demographic)
  } else if(cat %in% c("race", races)){
    df %<>% rename(category = race)
  } else if(cat %in% c("sex", sexes)){
    df %<>% rename(category = sex)
  }
  
  # Create category names
  if(cat %in% c("", races, sexes)){
    cat_names <- ""
  } else if(cat == "sex"){
    cat_names <- sexes
  } else if(cat == "race"){
    cat_names <- races
    cat_names <- cat_names[cat_names %in% df$category]
    if(df_type(df) %in% c("county", "MSA") & !include_hispanic){
      cat_names <- cat_names[cat_names != "hispanic"]
    }
  } else if(cat == "frl"){
    cat_names <- c("frl", "nonfrl")
  }
  
  # Filter to category or categories
  if(df_type(df) %in% c("county", "MSA")){
    if(cat %!in% c("sex", sexes)) df %<>% filter(sex == "total")
    if(cat %!in% c("race", races)) df %<>% filter(race == "total")
  }
  
  if(cat %in% c("race", "sex", "frl")){
    df %<>% filter(category %in% cat_names)
  } else if (cat %in% c(races, sexes)){
    df %<>% filter(category == cat) %>% select(-category)
  } else if(df_type(df) %in% c("ky_ed", "naep")){
    df %<>% filter(category == "total") %>% select(-category)
  }
  
  if(return_only_df){
    df
  } else {
    make_list(df, cat_names)
  }
}

#' Reshape data to long format
#' 
#' @return A data frame containing the columns year, --category--, variable, and value.
tl_reshape_data <- function(df){
  
  # If data frame is NAEP, simply reformat data frame and return.
  if(df_type(df) == "naep"){
    df %<>% 
      mutate(
        variable = replace(geography, geography == "Jefferson County", "lou"),
        variable = replace(variable, variable == "Kentucky", "mean"),
        value = var)
    
    df %<>%
      select(df %cols_in% c("year", "category", "variable", "value"))
    
    return(df)
  }
  
  grouping_vars <- c("year", "category")[c("year", "category") %in% names(df)]
  
  # Create a data frame with Louisville and 
  #   a data frame without Louisville.
  if(df_type(df) =="ky_ed"){
    df_wol <- df %>% filter(district != "Jefferson County")
    lville <- df %>% filter(district == "Jefferson County")
  } else {
    df_wol <- df %>% filter(FIPS != 21111)
    lville <- df %>% filter(FIPS == 21111)
  }
  
  #  Group the data set by year, category, if avilable.
  #  Calculate the 25th percentile, 75th percentile, 
  #   and mean of the peer data set.
  df_wol %<>%
    group_by_at(grouping_vars) %>%
    summarise(q1 = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              q3 = quantile(var, prob = 0.75, na.rm = TRUE))
  
  # Lville data frame contains three columns
  lville %<>%
    select(lville %cols_in% c("year", "category", "var")) %>%
    rename(lou = var)
  
  #join 25th percentile, 75th percentile, and Louisville values
  df <- full_join(lville, df_wol, by = grouping_vars)
  
  #Reshape the data to long format
  df %<>% gather(lou, q1, mean, q3, key = "variable", value = "value")
  
  df
}
    
#' Reshape data to long format for trendlines displaying the best- and worst-performing peer cities.
#' 
#' @return A data frame with the columns year, city, variable, category, and value
tl_reshape_data_maxmin <- function(df, 
                                   xmin, 
                                   xmax, 
                                   order){
  
  # Calculate change from xmin
  df %<>%
    arrange(FIPS, year) %>%
    filter(year >= xmin & year <= xmax) %>%
    group_by(FIPS) %>%
    mutate(change = var - first(var)) %>%
    ungroup() %>%
    select(year, city, var, change)
  
  #if(same_start){df$var <- df$change}

  # Is the "best" city the one with the largest positive 
  #   growth or largest negative growth?
  if(order %in% c("ascending", "Ascending")){
    max_change <- "best"
    min_change <- "worst"
  } else if(order %in% c("descending", "Descending")){
    max_change <- "worst"
    min_change <- "best"
  }
  
  # Calculate which peers had the best and worst change.
  city_list <- df %>%
    filter(year == xmax) %>%
    filter(change == max(change, na.rm = TRUE) | change == min(change, na.rm = TRUE) | city == "Louisville") %>%
    mutate(category = case_when(
      change == max(change, na.rm = TRUE) ~ max_change,
      change == min(change, na.rm = TRUE) ~ min_change,
      city == "Louisville" ~ "Louisville",
      TRUE ~ ""))  %>%
    select(city, category)
  
  #calculate peer city mean
  peer_mean <- df %>%
    filter(city != "Louisville") %>%
    group_by(year) %>%
    summarise(var = mean(var, na.rm = TRUE))%>%
    mutate(category = "peer_mean")
  
  df <- df %>%
    right_join(city_list, by = "city") %>%
    bind_rows(peer_mean) %>% 
    rename(
      variable = category,
      value = var) %>%
    select(-change)
  
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
                            return_only_df = F){
  
  # census = TRUE if 2000 is in the data frame bu 2001:2004 are not.
  census <- 2000 %in% years_in_df(df) & 
            all(2001:2004 %!in% years_in_df(df))
  
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
  }
  
  # Decrease xmax (regardless of 2000 census)
  xmax <- xmax - floor(rollmean / 2)
  
  # Filter to appropriate years based on the 
  #   rolling mean and remove any other NA values.
  df %<>%
    filter(year >= xmin) %>%
    filter(year <= xmax) %>%
    filter(!is.na(value))
  
  # Adjust subtitle or create a new subtitle to 
  #   refernce the rolling mean.
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

#' Add factor columns for ggplot to reference when assigning line groupings and styles
#'
#' @return A data frame
tl_add_line_data <- function(df, type, cat_names, 
                             drop_pctiles){
  
  # line_group groups data based on which points should be connected by a line. (Each line is unique.)
  # style_group groups data based on aesthetic. (Percentile lines are identical in style.)
  
  # If drop_pctiles, remove all percentile data
  if(drop_pctiles){
    df %<>% filter(variable %!in% c("q1", "q3"))
  }
  
  # Create line_group by combining category (e.g. male, female) and variable (lou, q1, mean, q3) and factoring.
  # If no categories are included, line_group is equivalent to variable.
  # variable is initially named style_group but is recoded below.
  if("category" %in% names(df)){
    df$style_group <- paste0(df$category, "_", df$variable)
  } else {
    df$style_group <- df$variable
  }

  df %<>% mutate(line_group = factor(style_group))

  # Label and factor style_group with the order based on cat_names.
  
  # var_levels matches the values of style_group
  # var_names to use in the legend in the appropriate order
  # factor style_group using var_levels and var_names
  
  if(cat_names[1] != ""){
    cat_levels <- cat_names %>% paste0("_")
    cat_names <- cat_names %>% paste0(" ") %>% str_to_title
  } else {
    cat_levels <- ""
  }
  
  if(cat_names[1] == "Frl "){
    cat_names <- c("FRL ", "non-FRL ")
  }
  
  var_levels <- c(paste0(cat_levels, "lou"),
                  paste0(cat_levels, "mean"),
                  paste0(cat_levels, "q1"),
                  paste0(cat_levels, "q3"))
  
  #Text for the legend is either "Peer" or "KY" based on the data type.
  if(type == "kentucky"){
    group <- "KY"
  } else {
    group <- "Peer"
  }
  
  var_labels <- c(paste0(cat_names, "Louisville"),
                  paste0(cat_names, group, " Mean"),
                  rep("25th and 75th Percentiles", 2 * length(cat_names)))
  
  #If drop_pctiles, remove percentile portions of vectors
  if(drop_pctiles){
    var_levels <- var_levels[1:(2*length(cat_levels))]
    var_labels <- var_labels[1:(2*length(cat_names))]
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
  
  var_levels <- c("Louisville", "best", "worst", "peer_mean")
  
  var_labels <- c("Louisville",
                  paste0("Most improved: ", unique(df$city[df$variable == "best"])),
                  paste0("Least improved: ", unique(df$city[df$variable == "worst"])),
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
  #If 5 or fewers years of data displayed, show every year on x-axis
  #Otherwise, show every other year on x-axis
  if(xmax - xmin <= 5){
    skip = 1
  } else {
    skip = 2
  }

  # census = TRUE if 2000 is in the data frame bu 2001:2004 are not.
  census <- 2000 %in% years_in_df(df) & 
    all(2001:2004 %!in% years_in_df(df))
  
  #If 2000 is included and 2001 is not, then skip interim years. By default, 
  #a minor break will occur between 2000 and the first year of ACS data. Remove that line.
  #Otherwise, include every year.
  if(census){
    major_break_settings <- c(2000, seq(2005 + floor(rollmean / 2), xmax, skip))
    minor_break_settings <- seq(2005 + floor(rollmean / 2) + 1, xmax - 1, skip)
  } else {

    #If there are an odd number of years and every other year is displayed,
    #show the most recent year
    if((xmax - xmin) %% 2 == 1 & skip == 2){
      major_break_settings = seq(xmin + 1, xmax, skip)
      minor_break_settings = waiver()
    }
    else{
      major_break_settings = seq(xmin, xmax, skip)
      minor_break_settings = waiver()
    }
  }

  make_list(major_break_settings, minor_break_settings)
}

#' Initial plot
#'
#' @param df A data frame
#' @return A ggplot object
tl_plot<- function(df){
  p <- ggplot(data = df,
              aes(x = year, y = value,
                  group = line_group,
                  colour = style_group,
                  linetype = style_group,
                  alpha = style_group))
  txt_scale <- 2
  
  p <- p +
    geom_point(size = 2 * txt_scale) +
    geom_line(size = 1  * txt_scale)

  p
}

#' Add x and y axis limits
#'
#' @return A ggplot object
tl_limits <- function(p, df, xmin, xmax, ylimits,
                      major_break_settings, minor_break_settings){

  if(length(ylimits) == 1){
    midpoint <- (max(df$value, na.rm = TRUE) +
                 min(df$value, na.rm = TRUE))/2
  
    border_space <- abs(.1 * midpoint)
  
    ylimits <- c(min(df$value, na.rm = TRUE) - border_space,
                 max(df$value, na.rm = TRUE) + border_space)
  }
  
  p <- p +
    scale_x_continuous(
      limits = c(xmin, xmax),
      breaks = major_break_settings,
      minor_breaks = minor_break_settings) +
    scale_y_continuous(
      limits = ylimits,
      labels = comma)

  p
}

#' Add style elements
#'
#' @return A ggplot object
tl_style <- function(p, plot_title, y_title,
                     caption_text, subtitle_text,
                     cat_names){

  txt_scale <- 2
  
  #adjust theme
  p <- p + theme_bw(
    base_size = 11 * txt_scale, 
                    base_family = "Museo Sans 300")

  p <- p + theme(
    legend.title     = element_blank(),
    legend.position  = "top",
    legend.margin    = margin(t = 0.4 * txt_scale, unit = "cm"),
    legend.spacing.x = unit(0.4 * txt_scale, "cm"),
    legend.text      = element_text(size = 20 * txt_scale,
                                    margin = margin(b = 0.2 * txt_scale, t = 0.2 * txt_scale, unit = "cm")),
    
    axis.text    = element_text(size = 24 * txt_scale),
    axis.title   = element_text(size = 30 * txt_scale),
    axis.title.x = element_text(margin = margin(t = 0.2 * txt_scale, unit = "cm")),
    axis.title.y = element_text(margin = margin(r = 0.2 * txt_scale, unit = "cm")),

    plot.title = element_text(size = 42 * txt_scale,
                              hjust = .5,
                              margin = margin(b = 0.4 * txt_scale, unit = "cm")),
    
    plot.caption = element_text(size = 18 * txt_scale,
                                lineheight = 0.5))
  

  #add labels
  p <- p + labs(
    title   = plot_title,
    x       = "Year",
    y       = y_title,
    caption = caption_text)

  #add subtitle if included
  if(subtitle_text != ""){
    p <- p +
      theme(plot.subtitle = element_text(hjust = 0.5, size = 24 * txt_scale)) +
      labs(subtitle = subtitle_text)
  }
  
  # If two rows of legend entries will be displayed, align the categories vertically.
  if(length(cat_names) >= 4){
    p <- p + guides(colour = guide_legend(label.position = "top",
                                          byrow = TRUE,
                                          keywidth = unit(6 * txt_scale, "lines")))
  } else {
    p <- p + guides(colour = guide_legend(label.position = "top"))
  }
  
  p
}

#' Add lines
#'
#' @return A ggplot object
tl_lines <- function(p, df, shading, cat_names, drop_pctiles){

  #Extract stlyle labels to match setting with legend
  line_types <- levels(df$style_group)

  #caluculate number of categories
  v <- (length(cat_names))

  #set line palette
  if(v == 1){
    line_col   <- c("#00a9b7", "black", "grey50")
  } else {
    #pal <- c("blue", "red", "darkgreen", "purple")[1:v]
    pal <- brewer_pal("qual", "Set1")(v)
    if(drop_pctiles){
      line_col   <- c(rep(pal, 2))
    } else {
      line_col   <- c(rep(pal, 2), "grey50")
    }
  }

  #set line type
  if(drop_pctiles){
    line_type  <- c(rep("solid", v), rep("dashed", v))
  } else {
    line_type  <- c(rep("solid", v), rep("dashed", v + 1))
  }
  

  #set line alphas. If shading is used, remove percentile lines
  if(shading){
    line_alpha <- c(rep(1, v), rep(0.7, v), 0)
  } else if (drop_pctiles){
    line_alpha <- c(rep(1, v), rep(0.7, v))
  } else {
    line_alpha <- c(rep(1, v), rep(0.7, v), 0.7)
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
    df$line_group <- unfactor(df$line_group)
    
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
