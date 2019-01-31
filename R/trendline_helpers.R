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
tl_filter <- function(df, peers ="current", sex ="total", race = "total", category = "") {
  
  #Filter to specified sex and race.
  #If sex or race are not ID variables, or if no sex or race was specified, do not subset.
  if("sex" %in% names(df)  & category != "sex")  df <- df[df$sex == sex,]
  if("race" %in% names(df) & category != "race") df <- df[df$race == race,]

  #Filter to peer set.
  if(peers %in% c("current", "Current")){
    df %<>% filter(current == 1)
  }

  if(peers %in% c("baseline", "Baseline")){
    df %<>% filter(baseline == 1)
  }

  df
}

#' Filter data to category and create category names. 
#' 
#' @return An object containing a data frame and category names
tl_filter_cat <- function(df, type = "single", 
                          category = "", include_hispanic = F){
  if(type == "multi"){
    if(category == "race"){
      if(include_hispanic){
        cat_names <- c("white", "black", "hispanic")
        df %<>% filter(race %in% cat_names)
      } else {
        cat_names <- c("white", "black")
        df %<>% filter(race %in% cat_names)
      }
    } else if (category == "sex"){
      cat_names = c("male", "female")
      df %<>% filter(sex %in% c("male", "female"))
    }
  } else if(type == "kentucky"){
    if(category == "total"){
      cat_names <- ""
      df %<>% filter(category == "all") %>% select(-category)
    } else if(category == "race"){
      cat_names <- c("white", "black", "hispanic", "asian")
      df %<>% filter(category %in% cat_names)
    } else if(category == "sex"){
      cat_names <- c("male", "female")
      df %<>% filter(category %in% cat_names)
    } else if(category == "frl"){
      cat_names <- c("frl", "nonfrl")
      df %<>% filter(category %in% cat_names)
    }
  } else {
    cat_names <- ""
  }
  make_list(df, cat_names)
}

#' Reshape data to long format
#' 
#' @return A data frame containing the columns year, variable, and value. If the data frame has categories, year, category, variable, and value.
tl_reshape_data <- function(df, type = "single", peers = "current"){
  
  #Create a data frame with Louisville and a data frame without Louisville
  if(type == "kentucky"){
    df_wol <- df %>% filter(district != "Jefferson County")
    lville <- df %>% filter(district == "Jefferson County")
  } else {
    df_wol <- df %>% filter(FIPS != 21111)
    lville <- df %>% filter(FIPS == 21111)
  }
  
  #Calculate the 25th percentile, 75th percentile, and mean of the peer data set
  #Group the data set by year and category, if avilable
  df_wol %<>%
    group_by_if(names(df_wol) %in% c("year", "category")) %>%
    summarise(q1 = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              q3 = quantile(var, prob = 0.75, na.rm = TRUE))
  
  #Rename the Louisville value to 
  lville %<>%
    select_if(names(df) %in% c("year", "category", "var")) %>%
    rename(lou = var)
  
  #join 25th percentile, 75th percentile, and Louisville values
  df <- full_join(lville, df_wol,
                  by = c("year", "category")[c("year", "category") %in% names(df_wol)])
  
  #Reshape the data to long format
  df %<>% gather(lou, q1, mean, q3, key = "variable", value = "value")
  
  df
}
    
#' Reshape data to long format for trendlines displaying the best- and worst-performing peer cities.
#' 
#' @return A data frame with the columns year, city, var, category, and value
tl_reshape_data_maxmin <- function(df, xmin = 2000, xmax = 2017, order = "descending"){
  df %<>%
    #organize df
    arrange(FIPS, year) %>%
    #select years
    filter(year >= xmin & year <= xmax) %>%
    #calculate change
    group_by(FIPS) %>%
    mutate(change = var - first(var)) %>%
    ungroup() %>%
    #select vars
    select(year, city, var, change)
  
  #if(same_start){df$var <- df$change}

  #Is the "best" city the one with the largest positive growth or largest negative growth?
  if(order %in% c("descending", "Descending")){
    max_change <- "best"
    min_change <- "worst"
  } else if(order %in% c("ascending", "Ascending")){
    max_change <- "worst"
    min_change <- "best"
  }
  
  #calculate which peers had the best and worst change
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
tl_rolling_mean <- function(df, xmin = 2000, xmax = 2017, rollmean = 1, census = T, subtitle_text = ""){
  #if 2000 census, split the data frame into 2000 and greater than 2000
  if(xmin == 2000 & census){
    df_2000 <- df %>% filter(year == 2000)
    df %<>% filter(year > 2000)
  }
  
  #use rolling mean function on non-2000 data frame
  df %<>%
    arrange(year) %>%
    group_by_if(names(df) %in% c("variable", "category")) %>%
    mutate(value = rollmeanr(value, rollmean)) %>%
    ungroup()
  
  #if 2000 census, reassemble data frame
  if(xmin == 2000 & census){
    df <- bind_rows(df_2000, df)
  }
  
  #if no 2000 census, increase xmin 
  if(!census){
    xmin <- xmin + floor(rollmean / 2)
  }
  
  #Decrease xmax (regardless of 2000 census)
  xmax <- xmax - floor(rollmean / 2)
  
  #filter to appropriate years based on the rolling mean and remove any other NA values
  df %<>%
    filter(year >= xmin) %>%
    filter(year <= xmax) %>%
    filter(!is.na(value))
  
  #adjust subtext or create a new subtitle to refernce the rolling mean
  if(rollmean > 1){
    if(subtitle_text == ""){
      subtitle_text <- paste0(rollmean,"-year rolling average")
    } else {
      subtitle_text <- paste0(subtitle_text, ", ", rollmean, "-year rolling average")}
  }
  
  
  make_list(df, xmin, xmax, subtitle_text)
}

#' Add factor columns for ggplot to reference when assigning line groupings and styles
#'
#' @return A data frame
tl_add_line_data <- function(df, type = "single", category = "", cat_names = "", 
                             drop_pctiles = F){
  
  #line_group groups data based on which points should be connected by a line. (Each line is unique.)
  #style_group groups data based on aesthetic. (Percentile lines are identical in style.)
  #If drop_pctiles, remove all percentile data
  if(drop_pctiles){
    df %<>% filter(variable %!in% c("q1", "q3"))
  }
  
  #Create line_group by combining category (e.g. male, female) and variable (lou, q1, mean, q3) and factoring.
  #If no categories are included, line_group is equivalent to variable.
  #variable is initially named style_group but is recoded below.
  if("category" %in% names(df)){
    df$style_group <- paste0(df$category, "_", df$variable)
  } else {
    df$style_group <- df$variable
  }

  df %<>% mutate(line_group = factor(style_group))

  #Factor style_group according to the order of entries in the legend (based on cat_names).
  
  #create var_levels to match the values of style_group
  #create var_names to use in the legend in the appropriate order
  #factor style_group using var_levels and var_names
  if(cat_names[1] != ""){
    cat_levels <- cat_names %>% paste0("_")
    cat_names <- cat_names %>% paste0(" ") %>% str_to_title
  } else {
    cat_levels <- ""
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
  
  if(cat_names[1] == "Frl "){
    cat_names <- c("FRL ", "non-FRL ")
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
tl_break_settings <- function(df, xmin = 2000, xmax = 2017, rollmean = 1, census = T){
  #If 5 or fewers years of data displayed, show every year on x-axis
  #Otherwise, show every other year on x-axis
  if(xmax - xmin <= 5){
    skip = 1
  } else {
    skip = 2
  }

  #If 2000 is included and 2001 is not, then skip interim years. By default, 
  #a minor break will occur between 2000 and the first year of ACS data. Remove that line.
  #Otherwise, include every year.
  if(xmin == 2000 & census){
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
  p <- p +
    geom_point(size = 2) +
    geom_line(size = 1)

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

  #adjust theme
  p <- p + theme_bw()

  p <- p + theme(
    text = element_text(family = "Museo Sans 300"),
    
    legend.title     = element_blank(),
    legend.position  = "top",
    legend.margin    = margin(t = 1, unit = "lines"),
    legend.spacing.x = unit(1.2, "lines"),
    legend.text      = element_text(size = 20),
    
    axis.text    = element_text(size = 24),
    axis.title   = element_text(size = 30),
    axis.title.x = element_text(margin = margin(t = 0.3, unit = "lines")),
    axis.title.y = element_text(margin = margin(r = 0.3, unit = "lines")),
    
    plot.title = element_text(size = 42,
                              hjust = .5,
                              margin = margin(b = 0.3, unit = "lines")),
    
    plot.caption = element_text(size = 18,
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
      theme(plot.subtitle = element_text(hjust = 0.5, size = 24)) +
      labs(subtitle = subtitle_text)
  }
  
  # If two rows of legend entries will be displayed, align the categories vertically.
  if(length(cat_names) >= 4){
    p <- p + guides(colour = guide_legend(label.position = "top",
                                          byrow = TRUE,
                                          keywidth = unit(6, "lines")))
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
    pal <- c("blue", "red", "darkgreen", "purple")[1:v]
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
    line_alpha <- c(rep(1, v), rep(0.6, v), 0)
  } else if (drop_pctiles){
    line_alpha <- c(rep(1, v), rep(0.6, v))
  } else {
    line_alpha <- c(rep(1, v), rep(0.6, v), 0.6)
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

  #set line palette
  line_col <- c("blue", "darkgreen", "red", "grey")
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
