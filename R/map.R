#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
make_map <- function(var, name, map_obj = "map_tract", geography = NULL, units = "Percent",
                     map_style = "sequential", legend_title = "", palette = "", reverse = F,
                     continuous = T, var_bins, tiles = TRUE,
                     save_data = F, save_file = F, save_image = F,
                     save_location){

  #assign map object to map. If parameter is blank, use the map_tract object
  assign("map", get(map_obj))

  if(is.null(geography)){
    geography <- strsplit(map_obj, "_", fixed = TRUE)[[1]][2]
  }

  #renames var for use with the '$' operator
  map@data$var <- map@data[[var]]

  two_line_geog <- c("neighborhood", "block", "zip", "market", "msa")

  #concatenate second or third line of text for tract labels using units parameter
  if(units == "Percent"){
    map@data$l_line3 <- paste(name, ": ", round(map@data$var, 2),"%", sep = "")
  } else if(units == "Dollars"){
    map@data$l_line3 <- paste(name, ": $",
                              prettyNum(
                                signif(map@data$var, 3),
                                big.mark = ",",
                                preserve.width = "none"
                              ),
                              sep = "")
  } else if(units == "none"){
    map@data$l_line3 <- paste(name, ": ", round(map@data$var, 2), sep = "")
  } else{
    map@data$l_line3 <- paste(name, ": ", round(map@data$var, 2), " ", units, sep = "")
  }

  #combine lines of text into full formatted label
  if(geography %in% two_line_geog){
    labels <- sprintf("%s<br/>%s",
                      map@data$l_line1,
                      map@data$l_line3) %>%
      lapply(htmltools::HTML)

    if(geography == "neighborhood"){
      labels[[which(map@data$neighborhood == "Airport")]] <- htmltools::HTML(sprintf("%s<br/>%s",
                                                                          "Louisville International Airport",
                                                                          "No residents"))
    }
  }else{
    labels <- sprintf("%s<br/>%s<br/>%s",
                      map@data$l_line1,
                      map@data$l_line2,
                      map@data$l_line3) %>%
      lapply(htmltools::HTML)

    labels[[which(map@data$neighborhood == "Airport")]] <- htmltools::HTML(sprintf("%s<br/>%s<br/>%s",
                                                                       "Tract #: 980000",
                                                                       "Louisville International Airport",
                                                                       "No residents"))
  }


  #Define palette using map_style parameter
  if(map_style == "sequential" | map_style == "Sequential"){
    col_palette = "BuPu"
  }
  if(map_style == "divergent" | map_style == "Divergent"){
    col_palette = "RdYlGn"
  }

  if(palette != ""){
    col_palette = palette
  }

  if(length(palette) == 1){
    pal <- brewer.pal(11, col_palette)
  } else {
    pal <- col_palette
  }

  if(reverse == T){
    pal <- rev(pal)
  }

  if(continuous){
    pal <- colorNumeric(
      palette = pal,
      domain = map@data$var
    )
  } else {
    pal <- colorBin(
      palette = pal,
      domain = map@data$var,
      bins = var_bins
    )
  }

  #Create map title using legend_title parameter
  if(units == "Percent") {
    title_text <- paste(legend_title, "(%)", sep = ' ')
  } else if(units == "Dollars") {
    title_text <- paste(legend_title, "($)", sep = ' ')
  } else if(units == "none"){
    title_text <- legend_title
  } else {
    title_text <- paste0(legend_title, " (", units, " )")
  }


  #create map
  if(tiles){
    m <- leaflet(map) %>%
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
  } else {
    m <- leaflet(map) %>%
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
  }
  
  if(save_data != F){
    name <- paste0(save_location, save_data, ".csv")
    write_csv(map@data, name)
  }
  if(save_file != F){
    name <- paste0(save_location, save_file)
    writeOGR(obj = map, dsn = name,
             layer = map_obj, driver = "ESRI Shapefile",
             overwrite_layer = TRUE)
  }
  if(save_image != F){
    name <- paste0(save_location, save_image, ".pdf")
    mapshot(m, file = name,
            zoom = 0.4, vwidth = 992 * 2, vheight = 744 * 2)
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
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2),"%", sep = "")
  }
  if(units == "Dollars"){
    map_jc@data$l_line3 <- paste(name, ": $",
                                 prettyNum(
                                   signif(map_jc@data$var, 3),
                                   big.mark = ",",
                                   preserve.width = "none"
                                 ),
                                 sep = "")
  }
  if(units == "minutes"){
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2)," minutes", sep = "")
  }
  if(units == "people"){
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2)," people", sep = "")
  }
  if(units == "none"){
    map_jc@data$l_line3 <- paste(name, ": ", round(map_jc@data$var, 2), sep = "")
  }

  #combine lines of text into full formatted label
  labels <- sprintf(map_jc@data$l_line3) %>%
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
