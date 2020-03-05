#' GLP MAp
#'
#' Creates a map of Louisville
#'
#' The parameters are organized into six sections:
#' \itemize{
#'   \item{\strong{Required}: \code{df}, \code{var}}
#'   \item{\strong{Filter and Adjust Data}:  \code{rollmean}, \code{xmin}, \code{xmax}, \code{peers}, \code{order}}
#'   \item{\strong{Demographics}:  \code{cat}, \code{include_hispanic}, \code{include_asian}}
#'   \item{\strong{Add Text to Graph}: \code{plot_title}, \code{y_title}, \code{caption_text}, \code{subtitle_text}}
#'   \item{\strong{Graph Elements}: \code{zero_start}, \code{ylimits}, \code{pctiles}, \code{shading}}
#'   \item{\strong{Manipulate Labels}: \code{label_function}, \code{axis_function}}
#' }
#'
#' @param maps A list of map files
#' @param var The variable name
#' @param hover_name The title of the variable for the hover box
#' @param legend_title The title for the legend, including <br> in place of line breaks
#'
#' @param units The units, defaults to \code{Percent}.
#' Specific formatting is applied for \code{Percent}, \code{Dollars}, and \code{none}.
#' Default blank formatting is applied for any other units.
#'
#' @param color_style,palette \code{color_style} creates a palette with sensible defaults:
#' \code{sequential} for a purple palette or \code{divergent} for a red/green palette.
#' \code{palette} can be used to replace \code{color_style} by providing a specific
#' \code{RColorBrewer::brewer.pal} function.
#'
#' @param reverse_pal Reverses the palette (particularly useful for switching a red-green palette).
#' @param continuous,var_bins Is the data continuous? Defaults to \code{TRUE}.
#'  If not, use \code{var_bins} to provide either the number of bins or a vector of cutpoints.
#'
#' @param tiles Include a map as a background for the leaflet map?
#' Can also provide \code{FALSE} to use a grey background.
#' Future: provide tile URL
#'
#' @param bold_nh If map of tracts, bold GLP neighborhood boundariess? Defaults to \code{TRUE}.
#'
#' @param save_file  Provide file destination to save leaflet file.
#' @param save_image Provide file destination (wihout file type) to save leaflet image as PDF.

#' @export
make_map <- function(maps, var,
                     hover_name, legend_title,
                     units = "Percent",
                     color_style = "sequential", palette = "", reverse_pal = F,
                     continuous = T, var_bins,
                     tiles = T,
                     bold_nh = T,
                     save_file = "", save_image = ""){

  if(typeof(maps) != "list") {maps <- list(maps)}

  # Get type of maps
  geographies <- map_chr(maps, df_type)

  # Rename variable to "var"
  maps %<>% map(function(obj) {obj@data$var <- obj@data[[var]]; obj})

  #concatenate second or third line of text for tract labels using units parameter
  line_1_2_fxn <- function(obj, geog) {
    if (geog == "tract") {
      obj@data %<>%
        mutate(
          line1 = paste0("Tract ", tract, " in the "),
          line2 = paste0(neighborhood, " neighborhood"))
    } else if (geog %in% c("nh", "muw")) {
      obj@data %<>%
        mutate(
          line1 = paste0(neighborhood, " neighborhood"))
    } else if (geog == "zip") {
      obj@data %<>%
        mutate(
          line1 = paste0("Zip code ", zip))
    }

    obj
  }

  maps %<>% map2(geographies, line_1_2_fxn)

  line3_fxn <- switch(units,
                      "Percent" = function(obj) {obj@data %<>% mutate(
                        line3 = paste0(hover_name, ": ", round(var, 2), "%")); obj},

                      "Dollars" = function(obj) {obj@data %<>% mutate(
                        line3 = paste0(hover_name, ": $", prettyNum(signif(var, 3),
                                                              big.mark = ",",
                                                              preserve.width = "none"))); obj},

                      "none" = function(obj) {obj@data %<>% mutate(
                        line3 = paste0(hover_name, ": ", round(var, 2))); obj},

                      function(obj) {obj@data %<>% mutate(
                        line3 = paste(hover_name, ": ", round(var, 2), " ", units)); obj})

  maps %<>% map(line3_fxn)

  #combine lines of text into full formatted label
  label_fxn <- function(obj) {

    if("line2" %in% names(obj@data)){
      labels <- sprintf("%s<br/>%s<br/>%s",
                        obj@data$line1,
                        obj@data$line2,
                        obj@data$line3) %>%
        lapply(htmltools::HTML)

      labels[[which(obj@data$neighborhood == "Airport")]] <-
        htmltools::HTML(sprintf("%s<br/>%s<br/>%s",
                                "Tract 98",
                                "Louisville International Airport",
                                "No residents"))
    } else {
      labels <- sprintf("%s<br/>%s",
                        obj@data$line1,
                        obj@data$line3) %>%
        lapply(htmltools::HTML)

      labels[[which(obj@data$neighborhood == "Airport")]] <-
        htmltools::HTML(sprintf("%s<br/>%s",
                                "Louisville International Airport",
                                "No residents"))
    }
    labels
  }

  labels <- map(maps, label_fxn)

  #Define palette using color_style parameter
  if (palette != "") {
    color_vector <- col_palette
  } else if (color_style %in% c("sequential", "Sequential")) {
    color_vector <- RColorBrewer::brewer.pal(9, "BuPu")
  } else if (color_style %in% c("divergent", "Divergent")) {
    color_vector <- RColorBrewer::brewer.pal(11, "RdYlGn")
  }

  if (reverse_pal) pal <- rev(pal)

  var_range <- map(maps, function(obj) range(obj@data$var, na.rm = T)) %>%
    range()

  if (continuous) {
    pal <- leaflet::colorNumeric(
      palette = color_vector,
      domain = var_range)
  } else {
    pal <- leaflet::colorBin(
      palette = color_vector,
      domain = var_range,
      bins = var_bins)
  }

  #Create map title using legend_title parameter
  title_text <- switch(units,
                       "Percent" = paste0(legend_title, " (%)"),
                       "Dollars" = paste0(legend_title, " ($)"),
                       "none"    = legend_title,
                                   paste0(legend_title, " (", units, " )"))

  #create map
  geographies %<>% recode(tract = "Census Tracts", nh = "GLP Neighborhoods",
                          muw = "United Way Neighborhoods")

  m <- leaflet()

  for (i in 1:length(maps)) {
    this_map  <- maps[[i]]
    this_geog <- geographies[[i]]
    these_labels <- labels[[i]]

    m <- m %>% addPolygons(
        data = this_map,
        color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~pal(var),
        label = these_labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "font-family" = "Arial", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        group = this_geog)

    if (this_geog == "Census Tracts" & bold_nh) {
      m <- m %>% addPolygons(
        data = map_nh,
        color = "#444444", weight = 2, smoothFactor = 0.5, opacity = 1.0, fill = FALSE,
        group = "tract")
    }
  }

  m <- m %>%
    addLegend(pal = pal, values = var_range, opacity = 0.7, title = title_text, position = "bottomright") %>%
    addLayersControl(baseGroups = geographies,
                     options = layersControlOptions(collapsed = F))

  #m <- m %>%
  #  hideGroup("GLP Neighborhoods") %>%
  #  showGroup("United Way Neighborhoods")

  if (tiles == TRUE) {
    m %<>% addTiles()
  } else if (tiles != FALSE) {
    m %<>% addTiles(urlTemplate = '//{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}.png')
  }

  # Fix legend NA value
  css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
  html_fix <- htmltools::tags$style(type = "text/css", css_fix)
  m %<>% htmlwidgets::prependContent(html_fix)

  if (save_file != "") {
    rgdal::writeOGR(obj = map, dsn = save_file,
                    layer = map_obj, driver = "ESRI Shapefile",
                    overwrite_layer = TRUE)
  }
  if (save_image != "") {
    mapview::mapshot(m, file = save_image %p% ".pdf", zoom = 0.4)
  }

  m
}


#' BRFSS
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
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
