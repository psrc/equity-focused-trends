library(psrccensus)
library(psrcelmer)
library(leaflet)
library(scales)
library(ggplot2)
library(leaflet.minicharts)
library(data.table)



create_tract_map_transit <- function(tract.lyr.acc,
                                         map.title = NULL, map.subtitle = NULL,
                                         map.title.position = "topright",
                                         legend.title = NULL, legend.subtitle = NULL,
                                         map.lat=47.615, map.lon=-122.257, 
                                         map.zoom=8.5, wgs84=4326){
  
  
  
 #tract.lyr.acc<-merge(tbl, tract.lyr.acc, by=c('tractgeoid_10'='geoid10'))
  

    
  
  color.ramp <- colorRamp(psrcplot::psrc_colors$purples_inc, interpolate="spline")
  pal <- leaflet::colorNumeric(palette=color.ramp, domain = tract.lyr.acc$access)
  
  tract.lyr.acc<-sf::st_as_sf(tract.lyr.acc)
  
  
  
  
  m <- leaflet::leaflet() %>%
    leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
    leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% 
    leaflet::addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    leaflet::addProviderTiles("CartoDB.VoyagerOnlyLabels",
                              options = leaflet::leafletOptions(pane = "maplabels"),
                              group = "Labels") %>%
    
    leaflet::addPolygons(data=tract.lyr.acc,
                         fillOpacity = 0.5,
                         fillColor = pal(tract.lyr.acc$access),
                         #fillColor = "white",
                         weight = 0.7,
                         #color = "#BCBEC0",
                         group="Transit Accessibility",
                         opacity = 0.1,
                         stroke=FALSE,
                         options = leaflet::leafletOptions(pane = "polygons"),
                         dashArray = "",
                         highlight = leaflet::highlightOptions(
                           weight =5,
                           color = "76787A",
                           dashArray ="",
                           fillOpacity = 0.9,
                           bringToFront = TRUE),
                         label = labels,
                         labelOptions = leaflet::labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto",
                           font="Poppins")) %>%
    
    leaflet::addLegend(pal = pal,
                       values = tract.lyr.acc$access,
                       labFormat = labelFormat(
                         suffix = "%",
                         transform = function(x) 100 * x
                       ),
                       position = "bottomright",
                       title = paste(legend.title, '<br>', legend.subtitle)) %>%
    
    leaflet::addLayersControl(baseGroups = "CartoDB.PositronNoLabels",
                              overlayGroups = c("Labels", "Population")) %>%
    
    
    leaflet::setView(lng=map.lon, lat=map.lat, zoom=map.zoom)
    
    
  
  return(m)
} 


### transit accessibilities (from Brice's data)
# path <- "L:/RTP_2022/final_runs/sc_rtp_2018_final/soundcast/outputs/access/transit_jobs_access_person.csv"
# t <- fread(path)
# str(t)
# sum_by_parcel <- t[, .(min_emptot = min(EMPTOT_P), 
#                        max_emptot = max(EMPTOT_P),
#                        hh_count = uniqueN(hhno),
#                        prsn_count = uniqueN(V1)), by=PARCELID]
# qry <- "select parcel_id,  tract_geoid10 from small_areas.parcel_dim where base_year = 2018"
# prcls <- psrcelmer::get_query(sql=qry)
# prcls <- sum_by_parcel %>%
#   merge(prcls, by.x="PARCELID", by.y="parcel_id")
# prcls <- as.data.table(prcls)
# prcls[, prsn_count_weight := prsn_count / sum(prsn_count), by=tract_geoid10]
# tract_accessibilities <- prcls[, .(access = sum(min_emptot * prsn_count_weight)), by=tract_geoid10]
# write.csv(tract_accessibilities ,'tract_accessibility_2018.csv')
tract_accessibilities<-read.csv('C:/GitHub/equity-focused-trends/hisp_heritage/2023/tract_accessibility_2018.csv')
#hisp_workers_and_accessibilities <- merge(hisp_workers_join, tract_accessibilities, by.x="GEOID", by.y="tract_geoid10")
tract_accessibilites<-tract_accessibilities%>%mutate(geoid10=as.character(tract_geoid10))
tract.lyr <- st_read_elmergeo('TRACT2010_NOWATER')%>%mutate(geoid10=as.character(geoid10))
tract.lyr.acc<-merge(tract_accessibilites, tract.lyr, by='geoid10')

my.map <- create_tract_map_transit(tract.lyr.acc)
my.map




