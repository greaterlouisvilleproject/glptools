#' Health Insurance
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
rank_and_nb_group<-function(df, var, order="Descending", peers="Current",
                            plot_title="", y_title = "Percent", caption_text = "",
                            sigfig = 3, num_dec = 1, text = TRUE, h_line = FALSE,
                            subtitle_text = "", thousands_comma = T){
  #SET GRAPH PARAMETERS

  #copy variable var to a new column for use with the '$' operator
  df$var <- df[[var]]

  #subset df to peer parameter
  if(peers=="Current"){
    df<-subset(df,current ==1)
  }
  if(peers=="Baseline"){
    df<-subset(df,baseline ==1)
  }

  #sort df according to order parameter
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }

  #create numbered city labels for left side of graph
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$city)
  d.graph<-cbind(d.rank,names)

  #set bar color
  breaks <- classIntervals(d.graph$var,3,style="jenks")
  d.graph$color <- NA
  d.graph$color[d.graph$var <= breaks$brks[2]] <- "green"
  d.graph$color[d.graph$var > breaks$brks[2] & d.graph$var <= breaks$brks[3]] <- "yellow"
  d.graph$color[d.graph$var > breaks$brks[3]] <- "red"

  #round numbers in graph according to sigfif, num_dec, and thousands_comma parameters
  thousands_char <- if_else(thousands_comma == TRUE, ",", "")
  d.graph$round <- d.graph$var %>%
    signif(digits = sigfig) %>%
    round(digits = num_dec) %>%
    format(big.mark = thousands_char)

  #Set text format, highlight and italicise Louisville text, highlight Louisville bar
  d.graph$textfont <- "Museo Sans 300"
  d.graph$textfont[d.graph$city == "Louisville"] <- "Museo Sans 300 Italic"
  d.graph$textcolor <- "black"
  d.graph$textcolor[d.graph$city == "Louisville"] <- "#00a9b7"
  d.graph$linecolor <- "white"
  d.graph$linecolor[d.graph$city == "Louisville"] <- "#00a9b7"

  #PLOT GRAPH

  #initial plot
  p <- ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                               y=var,fill=factor(color)))+guides(fill=FALSE)

  #add  bars
  p <- p+geom_bar(stat="identity",color=rev(d.graph$linecolor), size = 1)+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p <- p+scale_fill_manual(values=c("#96ca4f","#db2834","#ffd600"))
  }
  if(order=="Descending"){
    p <- p+scale_fill_manual(values=c("#db2834","#96ca4f","#ffd600"))
  }

  #add features
  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 plot.title = element_text(size = 36, hjust = 0.5),
                 axis.text.y = element_text(hjust=0, family = rev(d.graph$textfont),
                                            size=20, color = rev(d.graph$textcolor)),
                 axis.title.x = element_text(size = 24),
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(),
                 plot.subtitle = element_text(hjust = 0.5, size = 18))

  #add numeric labels to bars based on text parameter
  if (text == TRUE) {
    p <-
      p + geom_text(
        aes(label = round),
        hjust = 1.1,
        size = 8,
        family = "Museo Sans 300"
      )
  }

  #add vertical line to the left side of the bars based on the h_line parameter
  if (h_line == TRUE){
    p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 1)
  }
  p <- p+labs(title = plot_title, y= y_title,
              x = "", caption = caption_text,
              subtitle = subtitle_text)
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
rank_var <- function(df_original, variables, order = "Descending", peers="Current", new_vars = ""){
  
  #subset df to peer parameter
  if(peers=="Current"){
    df_original <- subset(df_original, current ==1)
  }
  if(peers=="Baseline"){
    df_original <- subset(df_original, baseline ==1)
  }
  
  n <- 1

  for(v in 1:length(variables)){
    df  <- df_original[,c("city", "year", variables[n])]
    
    df$var <- df[[variables[n]]]
    
    #sort df according to order parameter
    if(order=="Ascending"){
      df <- df %>% 
        group_by(year) %>%
        arrange(var) %>%
        mutate(rank = row_number()) %>%
        ungroup()
    } else if(order=="Descending"){
      df <- df %>% 
        group_by(year) %>%
        arrange(desc(var)) %>%
        mutate(rank = row_number()) %>%
        ungroup()
    }
    
    if(new_vars == ""){
      var_name <- paste0(variables[n], "_rank")
    } else {
      var_name <- new_vars[n]
    }

    df[[var_name]] <- df$rank
        
    df <- df %>% select(-var, -rank)
    
    #add the data frame to the output
    if(n == 1){
      output <- df
    }else{
      output <- full_join(output, df, by = c("city", "year"))
    }
    
    n = n + 1
    
  }
  
  output <- output %>%
    arrange(year, city)
  
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