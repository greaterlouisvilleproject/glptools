#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
make_list <- function(...){
  dots_content <- list(...)
  dots_names <- as.character(eval(substitute(alist(...))))
  setNames(dots_content, dots_names)
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_filter <- function(df, peers, sex, race, category) {
  #filter to specified sex and race
  if("sex" %in% names(df)  & category != "sex")  df <- df[df$sex == sex,]
  if("race" %in% names(df) & category != "race") df <- df[df$race == race,]

  #subset to peers
  if(peers %in% c("current", "Current")){
    df %<>% filter(current == 1)
  }

  if(peers %in% c("baseline", "Baseline")){
    df %<>% filter(baseline == 1)
  }

  df
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_filter_cat <- function(df, type, 
                          category, include_hispanic){
  if(type == "multi"){
    if(category == "race"){
      if(include_hispanic){
        cat_names <- c("White", "Black", "Hispanic")
        df %<>% filter(race %in% c("white", "black", "hispanic"))
      } else {
        cat_names <- c("White", "Black")
        df %<>% filter(race %in% c("white", "black"))
      }
    } else if (category == "sex"){
      cat_names = c("Male", "Female")
      df %<>% filter(sex %in% c("male", "female"))
    }
  }
  
  if(type == "kentucky"){
    if(category == "total"){
      cat_names <- ""
      df %<>% filter(category == "all") %>% select(-category)
    } else if(category == "race"){
      cat_names <- c("White", "Black", "Hispanic", "Asian")
      df %<>% filter(category %in% c("white", "black", "hispanic", "asian"))
    } else if(category == "sex"){
      cat_names <- c("Male", "Female")
      df %<>% filter(category %in% c("male", "female"))
    } else if(category == "frl"){
      cat_names <- c("FRL", "nonFRL")
      df %<>% filter(category %in% c("frl", "nonfrl"))
    }
  }
  make_list(df, cat_names)
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_reshape_data <- function(df, type, peers){
  #remove Louisville
  if(type == "kentucky"){
    df_wol <- df %>% filter(district != "Jefferson County")
    lville <- df %>% filter(district == "Jefferson County")
  } else {
    df_wol <- df %>% filter(FIPS != 21111)
    lville <- df %>% filter(FIPS == 21111)
  }
  
  #calculate 25th and 75th percentiles
  df_wol %<>%
    group_by_if(names(df_wol) %in% c("year", "category")) %>%
    summarise(q1 = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              q3 = quantile(var, prob = 0.75, na.rm = TRUE))
  
  #extract Louisville values and rename var to lou
  lville %<>%
    select_if(names(df) %in% c("year", "category", "var")) %>%
    rename(lou = var)
  
  #join 25th percentile, 75th percentile, and Louisville values
  df <- full_join(lville, df_wol,
                  by = c("year", "category")[c("year", "category") %in% names(df_wol)])
  
  df %<>% gather(lou, q1, mean, q3, key = "variable", value = "value")
  
  df
}
    
#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_reshape_data_maxmin <- function(df, xmin, xmax, order){
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
  
  #calculate which peers had the biggest change
  if(order %in% c("descending", "Descending")){
    max_change <- "best"
    min_change <- "worst"
  } else if(order %in% c("ascending", "Ascending")){
    max_change <- "worst"
    min_change <- "best"
  }
  
  city_list <- df %>%
    filter(year == xmax) %>%
    filter(change == max(change) | change == min(change) | city == "Louisville") %>%
    mutate(category = case_when(
      change == max(change) ~ max_change,
      change == min(change) ~ min_change,
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_rolling_mean <- function(...){
  
  obj_list <- make_list(...)
  
  df            <- obj_list[["df"]]
  xmin          <- obj_list[["xmin"]]
  xmax          <- obj_list[["xmax"]]
  rollmean      <- obj_list[["rollmean"]]
  census        <- obj_list[["census"]]
  subtitle_text <- obj_list[["subtitle_text"]]
  
  #if 2000 census, split data frame
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
  if(xmin != 2000){
    xmin <- xmin + floor(rollmean / 2)
  }
  
  #Decrease xmax
  xmax <- xmax - floor(rollmean / 2)
  
  #filter to years of interest and non-NA values
  df %<>%
    filter(year >= xmin) %>%
    filter(year <= xmax) %>%
    filter(!is.na(value))
  
  #add rolling mean subtext to existing subtitle or create new subtitle
  #PARENT ENVIRONMENT
  if(rollmean > 1){
    if(subtitle_text == ""){
      subtitle_text <- paste0(rollmean,"-year rolling average")
    } else {
      subtitle_text <- paste0(subtitle_text, ", ", rollmean, "-year rolling average")}
  }
  
  make_list(df, xmin, xmax, subtitle_text)
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_add_line_data <- function(df, type, category, cat_names, 
                             drop_pctiles){
  #style group is the type of line. Percentiles are identical in style.
  #line group is unique to each line
  if(drop_pctiles){
    df %<>% filter(variable %!in% c("q1", "q3"))
  }
  
  if("category" %in% names(df)){
    df$style_group <- paste0(df$category, "_", df$variable)
  } else {
    df$style_group <- df$variable
  }

  df %<>% mutate(line_group = factor(style_group))

  #factor all_groups according to the order it will appear in the legend.
  #If cat_names is blank, the following code does not append any text
  if(cat_names[1] != ""){
    cat_levels <- cat_names %>% paste0("_") %>% str_to_lower
    cat_names <- cat_names %>% paste0(" ") %>% str_to_title
  } else {
    cat_levels <- ""
  }

  var_levels <- c(paste0(cat_levels, "lou"),
                  paste0(cat_levels, "mean"),
                  paste0(cat_levels, "q1"),
                  paste0(cat_levels, "q3"))

  
  if(type == "kentucky"){
    group <- "KY"
  } else {
    group <- "Peer"
  }
  
  var_labels <- c(paste0(cat_names, "Louisville"),
                  paste0(cat_names, " ", group, " Mean"),
                  rep("25th and 75th Percentiles", 2 * length(cat_names)))
  
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_add_line_data_maxmin <- function(df){
  
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_break_settings <- function(...){

  obj_list <- make_list(...)

  df            <- obj_list[["df"]]
  xmin          <- obj_list[["xmin"]]
  xmax          <- obj_list[["xmax"]]
  rollmean      <- obj_list[["rollmean"]]

  #If 5 or fewers years of data displayed, show every year
  #Otherwise, show every other year
  if(xmax - xmin <= 5){
    skip = 1
  } else {
    skip = 2
  }

  #If 2000 is included and 2001 is not, then skip interim years
  #Otherwise, include every year
  if(xmin == 2000 & sum(df$year == 2001) == 0){
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
tl_limits <- function(p, df, xmin, xmax, major_break_settings, minor_break_settings){

  midpoint <- (max(df$value, na.rm = TRUE) +
                 min(df$value, na.rm = TRUE))/2

  border_space <- .1 * midpoint

  ylimits <- c(min(df$value, na.rm = TRUE) - border_space,
               max(df$value, na.rm=TRUE) + border_space)

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

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
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
  
  if(length(cat_names) >= 4){
    p <- p + guides(colour = guide_legend(label.position = "top",
                                          byrow = TRUE))
  } else {
    p <- p + guides(colour = guide_legend(label.position = "top"))
  }
  
  p
}

#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
#'
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
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' cat_function()
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
