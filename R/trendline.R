#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
graph_trendline<-function(df,var, plot_title="",y_title="Percent", peers = "Current",
                          caption_text = "", subtitle_text = "",
                          rollmean = 1, xmin = 2005, xmax = 2016,
                          break_settings = ""){

  #create a new variable to use var with the '$' operator
  df$var <- df[[var]]

  #if(is.na(df$var[df$year == 2016 & df$city == "Louisville"]))  xmax = 2015

  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }

  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }

  #calculate 25th and 75th percentiles
  output_wol = df %>%
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))

  #extract Louisville values
  lville = df %>%
    filter(FIPS == 21111) %>%
    select(var, year)

  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = "year")

  if(xmin == 2000){
    dat_2000 <- dat %>% filter(year == 2000)
    dat <- dat %>% filter(year > 2000)
  }
  
  dat <- dat %>%
    arrange(year) %>%
    mutate_at(vars(-year), rollmeanr, rollmean)
  
  if(xmin == 2000){
    dat <- bind_rows(dat_2000, dat)
  } else {
    xmin = xmin + floor(rollmean / 2)
    dat %>% filter(year >= xmin)
  }
  
  xmax = xmax - floor(rollmean / 2)
  dat <- dat %>% filter(year <= xmax)
  
  if(rollmean > 1){
    if(subtitle_text == ""){subtitle_text = paste0(rollmean,"-year rolling average")}
    else{subtitle_text = paste0(subtitle_text, ", ", rollmean,"-year rolling average")}
  }
    
  #use to write out 2016 child poverty statistics (also write out the line above filtering out 2016 data)
  #write_csv(dat, 'C:/Users/Harrison Kirby/Desktop/GLP/child_pov_output2.csv')

  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    
    if(xmin == 2000 & sum(dat$year == 2001) == 0){
      break_settings = c(2000, seq(2005 + floor(rollmean / 2), xmax, skip))
    }else{
      if((xmax - xmin) %% 2 == 0 || skip == 1){
        break_settings = seq(xmin, xmax, skip)
      }
      else{
        break_settings = seq(xmin + 1, xmax, skip)
      }
    }
  }

  #reshape data
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  data_long <- data_long[!is.na(data_long$value),]

  #initial line plot
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable, alpha=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)

  #add color and line types
  cPalette <- c("#00a9b7", "grey50", "black","grey50")#"#f44542", "black","#00a346")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "Louisville",
      "75th Percentile",
      "Peer City Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "Louisville",
        "75th Percentile",
        "Peer City Mean",
        "25th Percentile"
      )
    ) +
    scale_alpha_manual(
      values = c(1, .65, .8, .65),
      labels = c(
        "Louisville",
        "75th Percentile",
        "Peer City Mean",
        "25th Percentile"
      )
    )

  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             axis.title = element_text(size = 24),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 18))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
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
graph_trendline_change<-function(df,var, plot_title="",y_title = "", peers = "Current",
                                 caption_text = "", subtitle_text = "",
                                 same_start = F, xmin = 2005, xmax = 2016,
                                 break_settings = "", rollmean = 1, output = ""){

  newest_year <- xmax
  oldest_year <- xmin
  
  #create a new variable to use var with the '$' operator
  df$var <- df[[var]]

  #subset to peers
  if(peers=="Current"){
    df <- subset(df, current == 1)
  }

  if(peers=="Baseline"){
    df <- subset(df, baseline == 1)
  }

  df <- df %>%
    #organize df
    arrange(FIPS, year) %>%
    #select years
    filter(year >= oldest_year & year <= newest_year) %>%
    #calculate change
    group_by(FIPS) %>%
    mutate(change = var - first(var)) %>%
    ungroup() %>%
    #select vars
    select(year, city, var, change)
    
  
  #Replace var with change if lines should start at the same position
  if(same_start){df$var <- df$change}
  
  #write output
  if(output != ""){
    out <- df %>%
      group_by(city) %>%
      mutate(change = var - first(var)) %>%
      ungroup()
    
    write_csv(out, output)
  }

  #calculate which peers had the biggest change
  city_list <- df %>%
    filter(year == newest_year) %>%
    filter(change == max(change) | change == min(change) | city == "Louisville") %>%
    mutate(category = case_when(
      change == max(change) ~ "best",
      change == min(change) ~ "worst",
      city == "Louisville" ~ "Louisville",
      TRUE ~ ""))  %>%
    select(city, category)
  
  #calculate peer city mean
  peer_mean = df %>%
    filter(city != "Louisville") %>%
    group_by(year) %>%
    summarise(var = mean(var, na.rm = TRUE))%>%
    mutate(category = "peer_mean")

  df <- df %>%
    right_join(city_list, by = "city") %>%
    bind_rows(peer_mean)
  
  #Create Category factor. 
  #If Louisville is a min or max, remove the "Louisville" level and adjust colors accordingly.
  if(length(unique(df$category)) == 4){
    df$category <- factor(df$category, 
        levels = c("best", "worst", "Louisville", "peer_mean"),
        labels = factor_labels <- 
          c(paste0("Most improved: ", unique(df$city[df$category == "best"])),
            paste0("Least improved: ", unique(df$city[df$category == "worst"])),
            "Louisville",
            "Peer Mean"),
        ordered = TRUE)
    
    cat_colors <- c("darkgreen", "red", "blue", "grey")
  } else {
    df$category <- factor(df$category, 
        levels = c("best", "worst", "peer_mean"),
        labels = factor_labels <- 
          c(paste0("Most improved: ", unique(df$city[df$category == "best"])),
            paste0("Least improved: ", unique(df$city[df$category == "worst"])),
            "Peer Mean"),
        ordered = TRUE)
    
    cat_colors <- c("darkgreen", "red", "grey")
  }
  
  if(xmin == 2000){
    df_2000 <- df %>% filter(year == 2000)
    df <- df %>% filter(year > 2000)
  }
  
  df <- df %>%
    group_by(category) %>%
    mutate(var = rollmeanr(var, rollmean))
  
  if(xmin == 2000){
    df <- bind_rows(df_2000, df)
  } else {
    xmin = xmin + floor(rollmean / 2)
    df %>% filter(year >= xmin)
  }
  
  xmax = xmax - floor(rollmean / 2)
  df <- df %>% filter((year <= xmax))
  
  if(rollmean > 1){
    if(subtitle_text == ""){subtitle_text = paste0(rollmean,"-year rolling average")}
    else{subtitle_text = paste0(subtitle_text, ", ", rollmean,"-year rolling average")}
  }
  
  df <- df %>% filter(!is.na(var))
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    
    if(xmin == 2000 & sum(data$year == 2001) == 0){
      major_break_settings = c(2000, seq(2005 + floor(rollmean / 2), xmax, skip))
      minor_break_settings = seq(2005 + floor(rollmean / 2) + 1, xmax - 1, skip)
    }else{
      if((xmax - xmin) %% 2 == 0 || skip == 1){
        major_break_settings = seq(xmin, xmax, skip)
        minor_break_settings = waiver()
      }
      else{
        major_break_settings = seq(xmin + 1, xmax, skip)
        minor_break_settings = waiver()
      }
    }
  }
  
  #initial line plot
  p <- ggplot(data = df,aes(x = year, y = var, colour = category))+
    geom_point(size = 1.8) +
    geom_line(size = 1) + 
    scale_colour_manual(values = cat_colors) +
    geom_text_repel(aes(label = round(var, 1)), filter(df, year == xmax),
                    size = 10, show.legend = FALSE,
                    nudge_x = 0.25, direction = "y", hjust = 0)
  p <- p + theme_bw()
  midpoint <- (max(df$var, na.rm = TRUE) + min(df$var, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(df$var, na.rm = TRUE) - border_space, max(df$var, na.rm=TRUE) + border_space))
  p <- p + xlim(c(xmin, xmax))
  p <- p + scale_x_continuous(limits = c(xmin, xmax + 1), 
                              breaks = major_break_settings,
                              minor_breaks = minor_break_settings)

  #add remaining style and elements
  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 legend.title=element_blank(),
                 legend.position = "right",
                 axis.text = element_text(size = 24),
                 axis.title = element_text(size = 24),
                 axis.ticks.y = element_blank(),
                 plot.title=element_text(size = 36, hjust=.5,
                                         margin=margin(b = 10, unit="pt")),
                 legend.text=element_text(size = 24),
                 plot.subtitle = element_text(hjust = 0.5, size = 18))
  p <- p + labs(title=plot_title,x="Year",
                y=y_title, caption = caption_text, subtitle = subtitle_text)
  
  p
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
trendline_multiple <- function(df, plot_title = "", 
                               vars, cat_labels = c("White", "Black", "Hispanic"), 
                               y_title = "Percent", peers = "Current",
                               caption_text = "", subtitle_text = "",
                               rollmean = 1, xmin = 2005, xmax = 2016,
                               break_settings = "", shading = FALSE){
  
  v <- length(vars)
  
  cat_labels <- cat_labels[1:v]
  
  #extract Louisville values
  lville = df %>%
    filter(FIPS == 21111) %>%
    select(year, vars)
  
  #subset to peers and remove Louisville
  if(peers=="Current"){
    df_wol <- df %>% filter(current == 1 & FIPS!=21111)
  }
  
  if(peers=="Baseline"){
    df_wol <- df %>% filter(baseline == 1 & FIPS!=21111)
  }
  
  #subset data frames to variables of interest
  lville <- lville %>% select(year, vars)
  df_wol <- df_wol %>% select(year, vars)
  
  #calculate 25th and 75th percentiles
  output_wol = df_wol %>%
    group_by(year)
  
  q1 <- output_wol %>% summarise_all(funs(q1 = quantile), prob = 0.25, na.rm = TRUE)
  peer_mean <- output_wol %>% summarise_all(funs(mean = mean), na.rm = TRUE)
  q3 <- output_wol %>% summarise_all(funs(q3 = quantile), prob = 0.75, na.rm = TRUE)
  
  #join 25th percentile, peer mean, 75th percentile, and Louisville values
  dat = full_join(lville, q1, by = "year") %>%
    full_join(peer_mean, by = "year") %>%
    full_join(q3, by = "year")
  
  if(xmin == 2000){
    dat_2000 <- dat %>% filter(year == 2000)
    dat <- dat %>% filter(year > 2000)
  }
  
  dat <- dat %>%
    arrange(year) %>%
    mutate_at(vars(-year), rollmeanr, rollmean)
  
  if(xmin == 2000){
    dat <- bind_rows(dat_2000, dat)
  } else {
    xmin = xmin + floor(rollmean / 2)
    dat <- dat %>% filter(year >= xmin)
  }
  
  xmax = xmax - floor(rollmean / 2)
  dat <- dat %>% filter((year <= xmax))
  
  if(rollmean > 1){
    if(subtitle_text == ""){
      subtitle_text = paste0(rollmean,"-year rolling average")
    }else{
      subtitle_text = paste0(subtitle_text, ", ", rollmean,"-year rolling average")
    }
  }
  
  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    }
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }
  
  #reshape data
  data_long <- melt(dat, id="year")
  data_long <- data_long[!is.na(data_long$value),]
  
  data_long$line_group <- data_long$variable
  
  var_levels <- c(vars, 
                  paste0(vars, "_mean"),
                  paste0(vars, "_q1"),
                  paste0(vars, "_q3"))
  
  var_labels <- c(paste0(cat_labels, " Louisville"),
                  paste0(cat_labels, " Peer Mean"),
                  rep("25th and 75th Percentiles", 2 * v))
  
  data_long$variable <- factor(data_long$variable,
                               levels = var_levels,
                               labels = var_labels,
                               ordered = TRUE)
  
  #initial line plot
  p <- ggplot(data = data_long, 
              aes(x = year, y = value, 
                  color = variable, 
                  linetype = variable, 
                  alpha = variable)) +
    geom_point(size = 1.8) +
    geom_line(size = 1, aes(group = line_group))
  
  p <- p + theme_bw()
  
  midpoint <- (max(data_long$value, na.rm = TRUE) +
                 min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space,
                  max(data_long$value, na.rm=TRUE) + border_space))
  p <- p + scale_x_continuous(limits = c(xmin, xmax), 
                              breaks = break_settings)
  p <- p + scale_y_continuous(labels = comma)
  
  #add color and line types
  
  line_colors <- c('blue', 'red', 'darkgreen')[1:v]
  
  color_pal <- c(rep(line_colors, 2), "grey")
  linetypes <- c(rep("solid", v), rep("dashed", v + 1))
  alphas    <- c(rep(1, v), rep(.8, v), .6)
  
  if(shading){alphas <- c(rep(1, v), rep(.8, v), 0)}
  
  p <- p + scale_colour_manual(values = color_pal)
  
  p <- p + scale_linetype_manual(values = linetypes)
  
  p <- p + scale_alpha_manual(values = alphas)
  
  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "right",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  
  if(shading){
    data_long$line_group <- as.character(data_long$line_group)
    
    positions <- data.frame(
      variable = data_long$variable,
      grouping = substr(data_long$line_group, 1,
                        str_length(data_long$line_group) - 3),
      quartile = substr(data_long$line_group, 
                        str_length(data_long$line_group) - 1,
                        str_length(data_long$line_group)),
      year  = data_long$year,
      value  = data_long$value)
    
    positions <- positions %>%
      filter(variable == "25th and 75th Percentiles")
    
    q1 <- positions %>%
      filter(quartile == "q1") %>%
      arrange(grouping, year)
    
    q3 <- positions %>%
      filter(quartile == "q3") %>%
      arrange(grouping, desc(year))
    
    positions <- bind_rows(q1, q3)
    
    positions$grouping <- factor(positions$grouping,
      levels = vars,
      labels = cat_labels
    )
    
    p <- p + geom_polygon(data = positions,
                          aes(x = year, y = value, 
                              group = grouping, 
                              fill = factor(grouping), 
                              color = factor(grouping)),
                          col = NA, alpha = 0.3) +
      scale_fill_manual(values = line_colors)
  }
  
  p
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
graph_trendline_race_peer_two<-function(df,vars, plot_title="",y_title="Percent", peers = "Current",
                                        caption_text = "", subtitle_text = "",
                                        cat_labels = c("White", "Black"),
                                        rollmean = 1, xmin = 2005, xmax = 2016,
                                        break_settings = ""){

  #create a new variable to use var with the '$' operator
  df$var_white <- df[[vars[1]]]
  df$var_black <- df[[vars[2]]]

  #if(is.na(df$var[df$year == 2016 & df$city == "Louisville"]))  xmax = 2015

  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }

  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }

  #calculate 25th and 75th percentiles
  output_wol = df %>%
    group_by(year) %>%
    summarise(
      white_q1   = quantile(var_white, prob = 0.25, na.rm = TRUE),
      white_mean = mean(var_white, na.rm = TRUE),
      white_q3   = quantile(var_white, prob = 0.75, na.rm = TRUE),
      black_q1   = quantile(var_black, prob = 0.25, na.rm = TRUE),
      black_mean = mean(var_black, na.rm = TRUE),
      black_q3    = quantile(var_black, prob = 0.75, na.rm = TRUE))

  #extract Louisville values
  lville = df %>%
    filter(FIPS == 21111) %>%
    select(var_white, var_black, year)

  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = "year")

  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }

  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3), rollmean3)
    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }

    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3), rollmean5)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }

  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }

  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    }
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }

  #reshape data
  data_long <- melt(dat, id="year")
  data_long <- data_long[!is.na(data_long$value),]

  data_long$race[data_long$variable %in% c('var_white', 'white_q1', 'white_mean', 'white_q3')] <- cat_labels[1]
  data_long$race[data_long$variable %in% c('var_black', 'black_q1', 'black_mean', 'black_q3')] <- cat_labels[2]

  #initial line plot
  p <- ggplot(data=data_long,aes(x = year, y = value, color = variable, linetype = variable, alpha = variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1, aes(group = variable))
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)

  #var_white var_black var_hisp white_q1 white_mean white_q3 black_q1 black_mean blackq3 hisp_q1 hisp_mean hisp_q3

  #add color and line types
  cPalette <- c('blue', 'red',
                "black", "blue", "black",
                "black", "red", "black")
  p <- p +
    scale_colour_manual(
      values = cPalette)

  p <- p + scale_linetype_manual(
    values = c("solid", "solid",
               "dashed", "dashed", "dashed",
               "dashed", "dashed", "dashed"))

  p <- p + scale_alpha_manual(
    values = c(1, 1, .2, .8, .2,
               .2, .8, .2))

  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)

  ribbon_dat <- data_long %>%
    filter(variable %in% c('white_q1', 'black_q1', 'white_q3', 'black_q3'))

  positions <- data.frame(
    variable = ribbon_dat$variable,
    race = ribbon_dat$race,
    x  = ribbon_dat$year,
    y  = ribbon_dat$value)

  positions <- positions %>%
  {
    x <- .
    bind_rows(
      x %>% filter(variable %in% c('white_q1', 'black_q1')),
      x %>% filter(variable %in% c('white_q3', 'black_q3')) %>% arrange(desc(x)))
  }

  p <- p + geom_polygon(data = positions,
                        aes(x = x, y = y, group = race, fill = factor(race), color = factor(race)),
                        col = NA, alpha = 0.2)

  #p <- p + scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086'))

  p <- p + guides(linetype = FALSE, color = FALSE, alpha = FALSE)

  p
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
graph_trendline_mig<-function(df,vars, plot_title="",y_title="Percent", peers = "Current",
                              caption_text = "", subtitle_text = "",
                              rollmean = 1, xmin = 2005, xmax = 2016,
                              break_settings = ""){

  #create a new variable to use var with the '$' operator
  df$var_white <- df[[vars[1]]]
  df$var_black <- df[[vars[2]]]
  df$var_hisp  <- df[[vars[3]]]

  #if(is.na(df$var[df$year == 2016 & df$city == "Louisville"]))  xmax = 2015

  #subset to peers and remove Louisville
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }

  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }

  #calculate 25th and 75th percentiles
  output_wol = df %>%
    group_by(year) %>%
    summarise(
      white_q1   = quantile(var_white, prob = 0.25, na.rm = TRUE),
      white_mean = mean(var_white, na.rm = TRUE),
      white_q3   = quantile(var_white, prob = 0.75, na.rm = TRUE),
      black_q1   = quantile(var_black, prob = 0.25, na.rm = TRUE),
      black_mean = mean(var_black, na.rm = TRUE),
      black_q3    = quantile(var_black, prob = 0.75, na.rm = TRUE),
      hisp_q1    = quantile(var_hisp, prob = 0.25, na.rm = TRUE),
      hisp_mean  = mean(var_hisp, na.rm = TRUE),
      hisp_q3    = quantile(var_hisp, prob = 0.75, na.rm = TRUE))

  #extract Louisville values
  lville = df %>%
    filter(FIPS == 21111) %>%
    select(var_white, var_black, var_hisp, year)

  #join 25th percentile, 75th percentile, and Louisville values
  dat = full_join(lville, output_wol, by = "year")

  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }

  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black, var_hisp,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3,
                     hisp_q1, hisp_mean, hisp_q3), rollmean3)
    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }

    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat <- dat %>%
      mutate_at(vars(var_white, var_black, var_hisp,
                     white_q1, white_mean, white_q3,
                     black_q1, black_mean, black_q3,
                     hisp_q1, hisp_mean, hisp_q3), rollmean5)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }

  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }

  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    }
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }

  #reshape data
  data_long <- melt(dat, id="year")
  data_long <- data_long[!is.na(data_long$value),]

  data_long$race[data_long$variable %in% c('var_white', 'white_q1', 'white_mean', 'white_q3')] <- "Net Migration"
  data_long$race[data_long$variable %in% c('var_black', 'black_q1', 'black_mean', 'black_q3')] <- "Inmigration"
  data_long$race[data_long$variable %in% c('var_hisp', 'hisp_q1', 'hisp_mean', 'hisp_q3')] <- "Outmigration"

  #initial line plot
  p <- ggplot(data=data_long,aes(x=year, y=value, color = variable, linetype = variable, alpha = variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1, aes(group = variable))
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)

  #var_white var_black var_hisp white_q1 white_mean white_q3 black_q1 black_mean blackq3 hisp_q1 hisp_mean hisp_q3

  #add color and line types
  cPalette <- c('blue', 'red', 'darkgreen',
                "black", "blue", "black",
                "black", "red", "black",
                "black", "darkgreen", "black")
  p <- p +
    scale_colour_manual(
      values = cPalette)

  p <- p + scale_linetype_manual(
    values = c("solid", "solid", "solid",
               "dashed", "dashed", "dashed",
               "dashed", "dashed", "dashed",
               "dashed", "dashed", "dashed"))

  p <- p + scale_alpha_manual(
    values = c(1, 1, 1, 0, .8, 0,
               0, .8, 0, 0, .8, 0))

  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)

  ribbon_dat <- data_long %>%
    filter(variable %in% c('white_q1', 'black_q1', 'hisp_q1', 'white_q3', 'black_q3', 'hisp_q3'))

  positions <- data.frame(
    variable = ribbon_dat$variable,
    race = ribbon_dat$race,
    x  = ribbon_dat$year,
    y  = ribbon_dat$value)

  positions <- positions %>%
  {
    x <- .
    bind_rows(
      x %>% filter(variable %in% c('white_q1', 'black_q1', 'hisp_q1')),
      x %>% filter(variable %in% c('white_q3', 'black_q3', 'hisp_q3')) %>% arrange(desc(x)))
  }

  p <- p + geom_polygon(data = positions,
                        aes(x = x, y = y, group = race, fill = factor(race), color = factor(race)),
                        col = NA, alpha = 0.3)

  #p <- p + scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086'))

  p <- p + guides(linetype = FALSE, color = FALSE, alpha = FALSE)

  p
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
graph_trendline_race <- function(data_long, var = "var", value = "value", plot_title="",y_title="Percent",
                                 caption_text = "", subtitle_text = "", rollmean = 1,
                                 break_settings = "", xmin = 1996, xmax = 2016,
                                 labels, color_pal){
  data_long$var <- data_long[[var]]
  data_long$value<-data_long[[value]]
  data_long %<>% select(year, var, value)
  data_long <- arrange(data_long, as.character(var))

  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    }
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }

  p <- ggplot(data=data_long,aes(x=year,y=value,colour=var))+
    geom_point(size = 1.8)+
    geom_line(data=data_long[!is.na(data_long$value),], size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  p<-p+scale_y_continuous(labels = comma)
  cPalette <- color_pal
  p <- p + scale_colour_manual(values = cPalette, labels = labels)
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=24, family = "Museo Sans 300"),
             axis.title = element_text(size = 24),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=36, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=24, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
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
graph_trendline_lou<-function(df,var, plot_title="",y_title="Percent",
                              caption_text = "", subtitle_text = "",
                              rollmean = 1, xmin = 2005, xmax = 2016,
                              break_settings = ""){

  #create a new variable to use var with the '$' operator
  df$var <- df[[var]]

  #extract Louisville values
  lville = df %>%
    filter(FIPS == 21111) %>%
    select(var, year)

  #join 25th percentile, 75th percentile, and Louisville values
  dat = lville

  if(xmin == 2000 & rollmean == 3){
    var_2000 <- dat$var[dat$year == 2000]
    first_quarter_2000 <- dat$first_quarter[dat$year == 2000]
    mean_2000 <- dat$mean[dat$year == 2000]
    third_quarter_2000 <- dat$third_quarter[dat$year == 2000]
  }

  #Calculate 3- or 5-year rolling average
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)

    if(xmin != 2000){
      dat <- dat %>% filter((year > xmin))
      xmin = xmin +1
    }

    dat <- dat %>% filter((year < xmax))
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }

  if(xmin == 2000 & rollmean == 3){
    dat$var[dat$year == 2000] <- var_2000
    dat$first_quarter[dat$year == 2000] <- first_quarter_2000
    dat$mean[dat$year == 2000] <- mean_2000
    dat$third_quarter[dat$year == 2000] <- third_quarter_2000
  }

  #use to write out 2016 child poverty statistics (also write out the line above filtering out 2016 data)
  #write_csv(dat, 'C:/Users/Harrison Kirby/Desktop/GLP/child_pov_output2.csv')

  #set x-axis labels based on break_settings parameter
  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    }
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }

  #reshape data
  data_long <- melt(dat, id="year")
  data_long <- data_long[!is.na(data_long$value),]

  #initial line plot
  p <- ggplot(data=data_long,aes(x=year,y=value, color = variable))+
    geom_point(size = 1.8, show.legend = FALSE)+
    geom_line(size = 1, show.legend = FALSE)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + scale_y_continuous(labels = scales::comma,
                              limits = c(min(data_long$value, na.rm = TRUE) - border_space,
                                         max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)

  #add color and line types
  cPalette <- c("#00a9b7")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "Louisville"
    )
  )

  #add remaining style and elements
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5, size = 20))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
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
graph_trendline_msa<-function(df,var, plot_title="",y_title="Percent",
                              peers = "Current", caption_text = "",
                              subtitle_text = "", rollmean = 1, break_settings = "",
                              xmin = 2005, xmax = 2016){
  df$var <- df[[var]]
  df = df %>% filter(year != 2016)
  if(peers=="Current"){
    df.wol <- filter(df,current == 1 & MSA!=31140)
  }
  if(peers=="Baseline"){
    df.wol <- filter(df,baseline == 1 & MSA!=31140)
  }
  output_wol = df %>%
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  lville = df %>%
    filter(MSA == 31140) %>%
    select(var, year)
  dat = full_join(lville, output_wol, by = "year")
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter((year > xmin) & (year < xmax))
    xmin = xmin +1
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }

  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    }
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }

  dat
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value)+min(data_long$value))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value) - border_space, max(data_long$value + border_space)))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "Louisville",
      "75th Percentile",
      "Peer City Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "Louisville",
        "75th Percentile",
        "Peer City Mean",
        "25th Percentile"
      )
    )
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
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
graph_trendline_ky_ed<-function(df,var, plot_title="",y_title="Percent",
                                caption_text = "", subtitle_text = "", rollmean = 1,
                                break_settings = "", xmin = 2005, xmax = 2015){
  df$var <- df[[var]]
  output_wol = df %>%
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  lville = df %>%
    filter(area == "Louisville") %>%
    select(var, year)
  dat = full_join(lville, output_wol, by = "year")

  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter((year > xmin) & (year < xmax))
    xmin = xmin +1
    xmax = xmax -1
    subtitle_text = "3-year rolling average"
  }
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter((year > xmin+1) & (year < xmax-1))
    xmin = xmin + 2
    xmax = xmax - 2
    subtitle_text = "5-year rolling average"
  }

  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    }
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }

  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p <- p + scale_colour_manual(
    values = cPalette,
    labels = c(
      "JCPS",
      "75th Percentile",
      "KY School District Mean",
      "25th Percentile"
    )
  ) +
    scale_linetype_manual(
      values = c("solid", "dashed", "dashed", "dashed"),
      labels = c(
        "JCPS",
        "75th Percentile",
        "KY School District Mean",
        "25th Percentile"
      )
    )
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
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
ky_ed_data_long_trendline <- function(data_long, var = "var", value = "value", plot_title="",y_title="Percent",
                                      caption_text = "", subtitle_text = "", rollmean = 1,
                                      break_settings = "", xmin = 1996, xmax = 2016,
                                      labels, color_pal){
  data_long$var <- data_long[[var]]
  data_long$value<-data_long[[value]]
  data_long %<>% select(year, var, value)
  data_long <- arrange(data_long, as.character(var))

  if(break_settings == ""){
    if(xmax - xmin > 5) {skip = 2}
    else {skip = 1}
    if((xmax - xmin) %% 2 == 0 || skip == 1){
      break_settings = seq(xmin, xmax, skip)
    }
    else{
      break_settings = seq(xmin + 1, xmax, skip)
    }
  }

  p <- ggplot(data=data_long,aes(x=year,y=value,colour=var))+
    geom_point(size = 1.8)+
    geom_line(data=data_long[!is.na(data_long$value),], size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- color_pal
  p <- p + scale_colour_manual(values = cPalette, labels = labels)
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}
