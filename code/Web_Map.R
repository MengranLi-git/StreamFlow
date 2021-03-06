library(tidyverse)
library(leaflet)
library(rgeos)
library(data.table)
setwd("F:\\StreamFlow")
load("Stream.Rdata")

Stream <- Stream[Q >= 0]
Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]
setkey(Stream_plot, Region)
Stream_mean <- Stream_plot[, mean(Q), by = Region]
names(Stream_mean) <- c("huc2", "mean")
rm(Stream, Stream_plot)

bound <- sf::st_read(dsn = "F:/StreamFlow/wbdhu2_a_us_september2020.gdb")
bound <- subset(bound, !huc2 %in% c("19", "20", "21", "22"))
bound <- full_join(bound, Stream_mean, by = c("huc2" = "huc2"))

GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}
pal <- colorNumeric(
  palette = colorRampPalette(c("skyblue", "darkblue"))(length(bound$mean)),
  domain = bound$mean
)

leaflet(bound) %>%
  setView(lng = -95, lat = 40, zoom = 4) %>%
  addPolygons(
    color = ~ pal(mean), weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5, label = ~ htmltools::htmlEscape(paste(huc2, name)),
    highlightOptions = highlightOptions(
      color = "white", weight = 2,
      bringToFront = TRUE
    ),
    group = "Region"
  ) %>%
  addCircleMarkers(
    data = Siteinfo,
    lng = ~dec_lon_va,
    lat = ~dec_lat_va,
    radius = ~3,
    stroke = FALSE,
    fill = TRUE,
    color = "red",
    fillOpacity = 0.4,
    group = "Site"
  ) %>%
  addLegend("bottomright",
    pal = pal,
    values = ~mean,
    title = "Mean",
    labFormat = labelFormat(),
    opacity = 1,
    group = "Region"
  ) %>%
  addWMSTiles(GetURL("USGSHydroCached"), layers = "0", group = "River") %>%
  addProviderTiles("Esri.WorldImagery", group = "Topography") %>%
  addLayersControl(
    overlayGroups = c("River", "Topography", "Region", "Site"),
    options = layersControlOptions(collapsed = FALSE)
  )
