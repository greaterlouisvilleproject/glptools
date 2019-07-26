#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
make_map <- function(var, name, map, legend_title, units = "Percent",
                     map_style = "sequential", palette = "", reverse = F,
                     continuous = T, var_bins, tiles = T, bold_nh = T,
                     save_file = "", save_image = ""){

  map@data$var <- map@data[[var]]

  geography <- df_type(map@data)

  two_line_geog <- c("block", "neighborhood", "zip", "market", "county")

  #concatenate second or third line of text for tract labels using units parameter
  if (units == "Percent") {
    map@data$line3 <- paste0(name, ": ", round(map@data$var, 2), "%")
  } else if (units == "Dollars") {
    map@data$line3 <- paste0(name, ": $",
                              prettyNum(
                                signif(map@data$var, 3),
                                big.mark = ",",
                                preserve.width = "none"))
  } else if (units == "none") {
    map@data$line3 <- paste0(name, ": ", round(map@data$var, 2))
  } else {
    map@data$line3 <- paste(name, ": ", round(map@data$var, 2), " ", units)
  }

  #combine lines of text into full formatted label
  if (geography %in% two_line_geog) {
    labels <- sprintf("%s<br/>%s",
                      map@data$line1,
                      map@data$line3) %>%
      lapply(htmltools::HTML)

    if (geography == "neighborhood") {
      labels[[which(map@data$neighborhood == "Airport")]] <-
        htmltools::HTML(sprintf("%s<br/>%s",
                                "Louisville International Airport",
                                "No residents"))
    }
  } else {
    labels <- sprintf("%s<br/>%s<br/>%s",
                      map@data$line1,
                      map@data$line2,
                      map@data$line3) %>%
      lapply(htmltools::HTML)

    labels[[which(map@data$neighborhood == "Airport")]] <-
      htmltools::HTML(sprintf("%s<br/>%s<br/>%s",
                              "Tract 98",
                              "Louisville International Airport",
                              "No residents"))
  }

  #Define palette using map_style parameter
  if (palette != "") {
    color_vector <- col_palette
  } else if (map_style %in% c("sequential", "Sequential")) {
    color_vector <- brewer.pal(9, "BuPu")
  } else if (map_style %in% c("divergent", "Divergent")) {
    color_vector <- brewer.pal(11, "RdYlGn")
  }

  if (reverse == T) pal <- rev(pal)

  if (continuous) {
    pal <- colorNumeric(
      palette = color_vector,
      domain = map@data$var)
  } else {
    pal <- colorBin(
      palette = color_vector,
      domain = map@data$var,
      bins = var_bins)
  }

  #Create map title using legend_title parameter
  if (units == "Percent")     title_text <- paste0(legend_title, " (%)")
  else if(units == "Dollars") title_text <- paste0(legend_title, " ($)")
  else if(units == "none")    title_text <- legend_title
  else                        title_text <- paste0(legend_title, " (", units, " )")


  #create map
  m <- leaflet(map) %>%
    addPolygons(
      color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
      fillColor = ~pal(var),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", "font-family" = "Arial", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(pal = pal, values = ~var, opacity = 0.7, title = title_text, position = "bottomright")

  if (geography == "tract" & bold_nh) {
    m %<>%
      addPolygons(
        data = map_nh,
        color = "#444444", weight = 2, smoothFactor = 0.5, opacity = 1.0, fill = FALSE)
  }

  if (tiles == TRUE) {
    m %<>% addTiles()
  } else if (tiles != FALSE) {
    m %<>% addTiles(urlTemplate = '//{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}.png')
  }

  # Fix legend NA value
  m %<>% prependContent(tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"))

  if (save_file != "") {
    writeOGR(obj = map, dsn = save_file,
             layer = map_obj, driver = "ESRI Shapefile",
             overwrite_layer = TRUE)
  }
  if (save_image != "") {
    mapshot(m, file = save_image %p% ".pdf", zoom = 0.4)
  }

  m
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
make_map_00 <- function(var, name, units = "Percent",
                        map_style = "sequential", legend_title = ""){

  #renames var for use with the '$' operator
  map_jc@data$var <- map_jc@data[[var]]

  #concatenate third line of text for tract labels using units parameter
  if(units == "Percent"){
    map_jc@data$line3 <- paste(name, ": ", round(map_jc@data$var, 2),"%", sep = "")
  }
  if(units == "Dollars"){
    map_jc@data$line3 <- paste(name, ": $",
                                 prettyNum(
                                   signif(map_jc@data$var, 3),
                                   big.mark = ",",
                                   preserve.width = "none"
                                 ),
                                 sep = "")
  }
  if(units == "minutes"){
    map_jc@data$line3 <- paste(name, ": ", round(map_jc@data$var, 2)," minutes", sep = "")
  }
  if(units == "people"){
    map_jc@data$line3 <- paste(name, ": ", round(map_jc@data$var, 2)," people", sep = "")
  }
  if(units == "none"){
    map_jc@data$line3 <- paste(name, ": ", round(map_jc@data$var, 2), sep = "")
  }

  #combine lines of text into full formatted label
  labels <- sprintf(map_jc@data$line3) %>%
    lapply(htmltools::HTML)

  labels[[190]] <- htmltools::HTML(sprintf("%s", "No residents")
  )

  #Define palette using map_style parameter
  if(map_style == "sequential" | map_style == "Sequential"){
    col_palette = "BuPu"
  }
  if(map_style == "divergent" | map_style == "Divergent"){
    col_palette = "RdYlGn"
  }
  pal <- brewer.pal(11, col_palette)
  pal <- colorNumeric(
    palette = pal,
    domain = map_jc@data$var
  )

  #Create map title using legend_title parameter
  if(units == "Percent") {
    title_text <- paste(legend_title, "(%)", sep = ' ')
  }
  if(units == "Dollars") {
    title_text <- paste(legend_title, "($)", sep = ' ')
  }
  if(units == "minutes"){
    title_text <- paste(legend_title, "(minutes)", sep = ' ')
  }
  if(units == "people"){
    title_text <- paste(legend_title, "(people)", sep = ' ')
  }
  if(units == "none"){
    title_text <- legend_title
  }

  #create map
  m <- leaflet(map_jc) %>%
    addTiles() %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~pal(var),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addLegend(pal = pal, values = ~var, opacity = 0.7, title = title_text,
              position = "bottomright")

  m
}
