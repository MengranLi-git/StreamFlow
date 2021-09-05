library(EGRET)
library(dataRetrieval)
library(tidyverse)
library(leaflet)
library(rgeos)
library(data.table)

load("F:/StreamFlow/joint_fitting/joint.Rdata")

bound <- sf::st_read(dsn = "F:/StreamFlow/wbdhu2_a_us_september2020.gdb")

GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

bound %>% filter(huc2=="07") %>%
  leaflet() %>%
  #  setView(lng = -95, lat = 40, zoom = 4) %>%
  addCircleMarkers(
    data = new %>% mutate(index = 1:27),
    lng = ~dec_long_va,
    lat = ~dec_lat_va,
    radius = ~3,
    label = ~ htmltools::htmlEscape(index),
    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),
    stroke = FALSE,
    fill = TRUE,
    color = "red",
    fillOpacity = 0.4,
  )  %>%
  addWMSTiles(GetURL("USGSHydroCached"), layers = "0")

# display name
library(stringr)
new <- new %>% select(station_nm, site_no, dec_lat_va, dec_long_va) %>%
  mutate(station_nm = str_to_lower(station_nm) )















