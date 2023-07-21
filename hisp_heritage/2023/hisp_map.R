library(psrccensus)
library(psrcelmer)
library(leaflet)
library(scales)


create_tract_map_transit <- function(census.tbl, census.lyr,
                                         map.title = NULL, map.subtitle = NULL,
                                         map.title.position = "topright",
                                         legend.title = NULL, legend.subtitle = NULL,
                                         map.lat=47.615, map.lon=-122.257, 
                                         map.zoom=8.5, wgs84=4326){
  
  
  tbl <- census.tbl 
  
  c.layer <- dplyr::left_join(census.lyr,census.tbl, by = c('geoid20'='GEOID')) %>%
    sf::st_transform(wgs84) %>%
    mutate(sym.radius = estimate_all / 50)
  
  color.ramp <- colorRamp(psrcplot::psrc_colors$purples_inc, interpolate="spline")
  pal <- leaflet::colorNumeric(palette=color.ramp, domain = c.layer$transit_share)
  
  
  labels <- paste0( "Commute Share: Hispanic: ", 
                    scales::percent(c.layer$transit_share, accuracy=1)) %>% 
    lapply(htmltools::HTML)
  
  
  
  
  m <- leaflet::leaflet() %>%
    leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
    leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% 
    leaflet::addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    leaflet::addProviderTiles("CartoDB.VoyagerOnlyLabels",
                              options = leaflet::leafletOptions(pane = "maplabels"),
                              group = "Labels") %>%
    
    leaflet::addEasyButton(leaflet::easyButton(icon="fa-globe",
                                               title="Region",
                                               onClick=leaflet::JS("function(btn, map)
                                               {map.setView([47.615,-122.257],8.5); }"))) %>%
    
    leaflet::addCircleMarkers(
      data = c.layer,
      lat = c.layer$ycoord,
      lng = c.layer$xcoord,
      radius = c.layer$sym.radius,
      color = pal(c.layer$transit_share),
      fill = TRUE,
      fillOpacity = 1
    ) %>%
    
    leaflet::addPolygons(data=c.layer,
                         fillOpacity = 0.1,
                         #fillColor = pal(c.layer$transit_share),
                         fillColor = "white",
                         weight = 0.7,
                         color = "#BCBEC0",
                         group="Population",
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
                       values = c.layer$transit_share,
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

means_transport_work_hisp <- psrccensus::get_acs_recs(geography='tract',
                                         table.names=c('B08105I'),years=c(2021))
#> Getting data from the 2017-2021 5-year ACS
#> Loading ACS5 variables for 2021 from table B08006. To cache this dataset for faster access to ACS tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per ACS dataset.
all_hisp_tract<-means_transport_work_hisp %>%
  filter(label=='Estimate!!Total:')
hisp_transit_tract <- means_transport_work_hisp %>%
  filter(label=='Estimate!!Total:!!Public transportation (excluding taxicab)')

hisp_workers_join<-merge(all_hisp_tract, hisp_transit_tract, by= 'GEOID', 
                         suffixes=c('_all', '_transit'))

hisp_transit_share <- hisp_workers_join %>%
  mutate(transit_share =estimate_transit/estimate_all)

tract.lyr <- st_read_elmergeo('TRACT2020_NOWATER')


transit_share_map<-create_tract_map_transit(hisp_transit_share, tract.lyr)
transit_share_map

### scatter plot: Hispanic working population vs. Public transportation to work ###
bc

g.hisp <- ggplot(data=hisp_transit_share,
            aes(x=estimate_all, y=transit_share)) +
  geom_point(size=3, color="blue", alpha = 0.7)
g.hisp  


### scatter plot: Black working population vs. Public transportation to work ###
means_transport_work_black <- psrccensus::get_acs_recs(geography='tract',
                                         table.names=c('B08105B'),years=c(2021))
all_blk_tract <- means_transport_work_black %>%
  filter(label=='Estimate!!Total:')
blk_transit_tract <- means_transport_work_black %>%
  filter(label=='Estimate!!Total:!!Public transportation (excluding taxicab)')
blk_workers_join<-merge(all_blk_tract, blk_transit_tract, by= 'GEOID', 
                         suffixes=c('_all', '_transit'))
blk_transit_share <- blk_workers_join %>%
  mutate(transit_share =estimate_transit/estimate_all)
g.blk <- ggplot(data=blk_transit_share,
            aes(x=estimate_all, y=transit_share)) +
  geom_point(size=3, color="green", alpha = 0.7)
g.blk  
g.hisp


### plot both hispanic and black scatter plots simultaneously ###
blk_transit_share <- blk_transit_share %>%
  mutate(demog_group = 'black')
hisp_transit_share <- hisp_transit_share %>%
  mutate(demog_group = 'hispanic')
df_comb <- rbind(blk_transit_share, hisp_transit_share)
g.comb <- ggplot(data=df_comb,
            aes(x=estimate_all, y=transit_share, color = demog_group)) +
  geom_point(size=2, alpha = 0.7)
g.comb  
