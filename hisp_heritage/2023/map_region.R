create_psrc_map <- function(lyr,
                            lyr_data_field,
                            legend_title,
                            legend_subtitle,
                            psrc_col_pal,
                            map_lat = 47.615,
                            map_lon = -122.257,
                            map_zoom = 8.5,
                            wgs84 = 4326) {
  # psrc colors need more contrast to work
  pal <-
    leaflet::colorNumeric(palette = psrc_col_pal, domain = lyr_data_field)
  
  css_fix <-
    "div.info.legend.leaflet-control br {clear: both;} html * {font-family: Poppins !important;}" # CSS to correct spacing and font family
  html_fix <-
    htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
  
  
  labels <-
    paste0('Estimate: ', prettyNum(round(lyr_data_field,-1), big.mark = ",")) %>%
    lapply(htmltools::HTML)
  
  m <- leaflet::leaflet() %>%
    leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
    leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
    
    leaflet::addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    leaflet::addProviderTiles(
      "CartoDB.VoyagerOnlyLabels",
      options = leaflet::leafletOptions(pane = "maplabels"),
      group = "Labels"
    ) %>%
    leaflet::addEasyButton(leaflet::easyButton(
      icon = htmltools::span(class = "globe", htmltools::HTML("&#127758;")),  #&#127760; (another emoji option) #"fa-globe", (font awesome icon no longer works because of the conversion to Poppins font below)  
      title ="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }")))%>%
    leaflet::addPolygons(
      data = lyr,
      fillOpacity = 0.7,
      fillColor = pal(lyr_data_field),
      weight = 0.7,
      color = "#BCBEC0",
      group = "estimate",
      opacity = 0,
      stroke = FALSE,
      options = leaflet::leafletOptions(pane = "polygons"),
      dashArray = "",
      highlight = leaflet::highlightOptions(
        weight = 5,
        color = "76787A",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px",
          "font-family" = "Poppins"
        )
      )
    ) %>%
    
    leaflet::addLegend(
      pal = pal,
      values = lyr_data_field,
      position = "bottomright",
      title = paste(legend_title, '<br>', legend_subtitle)
    ) %>%
    
    leaflet::addLayersControl(baseGroups = "CartoDB.VoyagerNoLabels",
                              overlayGroups = c("Labels", "estimate")) %>%
    
    leaflet::setView(lng = map_lon, lat = map_lat, zoom = map_zoom) %>%
    htmlwidgets::prependContent(html_fix)      # Insert into leaflet HTML code
  
  return(m)
  
}


# https://stackoverflow.com/questions/40276569/reverse-order-in-r-leaflet-continuous-legend - to get legend high-low with correct color order
addLegend_decreasing <-
  function (map,
            position = c("topright", "bottomright", "bottomleft",
                         "topleft"),
            pal,
            values,
            na.label = "NA",
            bins = 7,
            colors,
            opacity = 0.5,
            labels = NULL,
            labFormat = labelFormat(),
            title = NULL,
            className = "info legend",
            layerId = NULL,
            group = NULL,
            data = getMapData(map),
            decreasing = FALSE) {
    position <- match.arg(position)
    type <- "unknown"
    na.color <- NULL
    extra <- NULL
    if (!missing(pal)) {
      if (!missing(colors))
        stop("You must provide either 'pal' or 'colors' (not both)")
      if (missing(title) && inherits(values, "formula"))
        title <- deparse(values[[2]])
      values <- evalFormula(values, data)
      type <- attr(pal, "colorType", exact = TRUE)
      args <- attr(pal, "colorArgs", exact = TRUE)
      na.color <- args$na.color
      if (!is.null(na.color) &&
          col2rgb(na.color, alpha = TRUE)[[4]] ==
          0) {
        na.color <- NULL
      }
      if (type != "numeric" && !missing(bins))
        warning("'bins' is ignored because the palette type is not numeric")
      if (type == "numeric") {
        cuts <- if (length(bins) == 1)
          pretty(values, bins)
        else
          bins
        
        if (length(bins) > 2)
          if (!all(abs(diff(bins, differences = 2)) <=
                   sqrt(.Machine$double.eps)))
            stop("The vector of breaks 'bins' must be equally spaced")
        n <- length(cuts)
        r <- range(values, na.rm = TRUE)
        cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
        n <- length(cuts)
        p <- (cuts - r[1]) / (r[2] - r[1])
        extra <- list(p_1 = p[1], p_n = p[n])
        p <- c("", paste0(100 * p, "%"), "")
        if (decreasing == TRUE) {
          colors <- pal(rev(c(r[1], cuts, r[2])))
          labels <- rev(labFormat(type = "numeric", cuts))
        } else{
          colors <- pal(c(r[1], cuts, r[2]))
          labels <- rev(labFormat(type = "numeric", cuts))
        }
        colors <- paste(colors, p, sep = " ", collapse = ", ")
        
      }
      else if (type == "bin") {
        cuts <- args$bins
        n <- length(cuts)
        mids <- (cuts[-1] + cuts[-n]) / 2
        if (decreasing == TRUE) {
          colors <- pal(rev(mids))
          labels <- rev(labFormat(type = "bin", cuts))
        } else{
          colors <- pal(mids)
          labels <- labFormat(type = "bin", cuts)
        }
        
      }
      else if (type == "quantile") {
        p <- args$probs
        n <- length(p)
        cuts <- quantile(values, probs = p, na.rm = TRUE)
        mids <- quantile(values, probs = (p[-1] + p[-n]) / 2,
                         na.rm = TRUE)
        if (decreasing == TRUE) {
          colors <- pal(rev(mids))
          labels <- rev(labFormat(type = "quantile", cuts, p))
        } else{
          colors <- pal(mids)
          labels <- labFormat(type = "quantile", cuts, p)
        }
      }
      else if (type == "factor") {
        v <- sort(unique(na.omit(values)))
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
        if (decreasing == TRUE) {
          colors <- pal(rev(v))
          labels <- rev(labFormat(type = "factor", v))
        } else{
          colors <- pal(v)
          labels <- labFormat(type = "factor", v)
        }
      }
      else
        stop("Palette function not supported")
      if (!any(is.na(values)))
        na.color <- NULL
    }
    else {
      if (length(colors) != length(labels))
        stop("'colors' and 'labels' must be of the same length")
    }
    legend <-
      list(
        colors = I(unname(colors)),
        labels = I(unname(labels)),
        na_color = na.color,
        na_label = na.label,
        opacity = opacity,
        position = position,
        type = type,
        title = title,
        extra = extra,
        layerId = layerId,
        className = className,
        group = group
      )
    invokeMethod(map, data, "addLegend", legend)
  }