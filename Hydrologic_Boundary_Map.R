library(ggplot2)
library(rgeos)
vessel <- sf::st_read(dsn = "F:/StreamFlow/wbdhu2_a_us_september2020.gdb")
vessel <- subset(vessel, !huc2 %in% c("19","20","21","22"))
ggplot() + 
  geom_sf(data = bound) + 
  ggtitle("Hydrologic Boundary Plot") + 
  coord_sf()

pal <- colorNumeric(
  palette = colorRampPalette(c("skyblue", "darkblue"))(length(bound$mean)),
  domain = bound$mean
)

leaflet(bound) %>%
  addPolygons(color = ~ pal(mean), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,label = ~htmltools::htmlEscape(paste(huc2, name)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))%>%
  addLegend("bottomright",
            pal = pal,
            values = ~mean,
            title = "Mean of each region",
            labFormat = labelFormat(),
            opacity = 1
  ) %>%
  addWMSTiles(GetURL("USGSHydroCached"),layers = "0")

Stream_summary_site <- Stream[, .(
  mean(Q, na.rm = TRUE),
  sd(Q, na.rm = TRUE),
  quantile(Q, 0.05, na.rm = TRUE),
  quantile(Q, 0.1, na.rm = TRUE),
  quantile(Q, 0.2, na.rm = TRUE),
  quantile(Q, 0.5, na.rm = TRUE),
  quantile(Q, 0.7, na.rm = TRUE),
  quantile(Q, 0.9, na.rm = TRUE),
  quantile(Q, 0.95, na.rm = TRUE),
  quantile(Q, 0.975, na.rm = TRUE),
  quantile(Q, 0.99, na.rm = TRUE)
), by = Site]
names(Stream_summary_site) <- c(
  "Site", "Mean", "Sd",
  paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99))
)
map_site <- Siteinfo[, c("site_no", "dec_lat_va", "dec_lon_va")]
names(map_site) <- c("Site", "lat", "long")

Stream_summary_site_map <- full_join(Stream_summary_site, map_site, by = c("Site" = "Site"))

pal <- colorNumeric(
  palette = colorRampPalette(c("green", "red"))(671*11),
  domain = c(Stream_summary_site_map[,-c(1,13,14)])
)

GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

Stream_summary_site_map %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~long,
    lat = ~lat,
    radius = ~3,
    stroke = FALSE,
    fill = TRUE,
    color = ~pal(q0.99),
    fillOpacity = 0.4,
    group = "Site"
  )%>%
  addCircleMarkers(
    lng = ~long,
    lat = ~lat,
    radius = ~1,
    stroke = FALSE,
    fill = TRUE,
    fillOpacity = 0,
    group = "No Site"
  )%>%
  addWMSTiles(GetURL("USGSHydroCached"),layers = "0")%>%
  addLayersControl(
    overlayGroups =c("Site", "No Site"),
    options = layersControlOptions(collapsed=FALSE)
  )










