############################## library package ################################
{
library(EGRET)
library(dataRetrieval)
library(data.table)
library(tidyverse)
library(KernSmooth)
library(leaflet)
library(maps)
}
rm()
head()
############################### read data #####################################

setwd("F:\\StreamFlow")

load("Siteinfo.Rdata")
load("Streamflow.Rdata")

gauge <- read.table("gauge_information.txt", header = FALSE)
names(gauge) <- gauge[1, ]
gauge <- gauge[-1, ]

Region_name <- read.table("Region_name.txt", header = FALSE, sep=",")
names(Region_name) <- Region_name[1, ]
Region_name <- Region_name[-1, ]

# This parameter refers to streamflow dataset
  parameterCd <- "00060"

{
  # EGRET
  Streamflow <- lapply(
    gauge[, 2], readNWISDaily, parameterCd,
    "1980-01-01", "2019-12-31"
  )

 # site information from filelist2
  siteInfo <- lapply(filelist2, attr, "siteInfo")

  names(siteInfo) <- paste0("S", gauge[, 2])
  invisible(lapply(names(siteInfo), function(x) assign(x, siteInfo[[x]], envir = .GlobalEnv)))
  Siteinfo <- Reduce(function(x, y) merge(x, y, by = names(S14301000), all.x = T, all.y = T), siteInfo)

  # read site information directly by readNWISsite
#  site <- readNWISsite(gauge[, 2])
#  site2 <- readNWISInfo(gauge[, 2], "00060", interactive = FALSE)
  }
############################### plot ##########################################

for (i in 1:length(Streamflow)) {
  Streamflow[[i]]$Region <- gauge[i, 1]
  Streamflow[[i]]$Site <- gauge[i, 2]
}

Stream <- rbindlist(Streamflow)[
  ,DecYear := floor(DecYear)] %>%
    filter(DecYear != 2020)
rm(Streamflow)
#head(Stream)

Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]#[
#  , waterYear := as.factor(waterYear)
#][
#  , Month := as.factor(Month)
#] %>%
#  na.omit()
setkey(Stream_plot, Region)

# Stream_1980 <- Stream_plot["1980"]

############### boxplot (finish)##############

# Boxplot of population

Stream_plot[, boxplot(Q ~ DecYear)]

# run more slowly
p <- ggplot(Stream_plot, aes(x = as.factor(DecYear), y = Q)) +
       geom_boxplot()
#p

# Boxplot grouped by region
Stream_plot[,DecYear:=as.factor(DecYear)] %>%
  ggplot(aes(x = DecYear, y = Q, colour = Region)) +
  geom_boxplot() +
  facet_wrap(~Region)

############### Smooth ########################
Stream_Series <- Stream_plot[, mean(Q, na.rm = TRUE), 
                             by = .(DecYear, Month)] 

H <- as.matrix(Stream_Series[,dpill(DecYear, V1), by = Month])

Stream_Series_fit <- list()
for (i in 1:12){
  data <- Stream_Series[Month==i]
  Year <- data$DecYear
  Q <- data$V1
  Stream_Series_fit[[i]] <- locpoly(Year, Q, bandwidth = H[i,2])
  Stream_Series_fit[[i]]$Month <- i
}

rbindlist(Stream_Series_fit)[
  ,Month:=as.factor(Month)] %>%
  ggplot(aes(x=x,y=y,group=Month,colour=Month))+
  geom_line()


############################### statistics ##########################

Stream_map <- Stream[, .(
  mean(Q), sd(Q),
  quantile(Q, 0.05, na.rm = TRUE),
  quantile(Q, 0.1, na.rm = TRUE),
  quantile(Q, 0.2, na.rm = TRUE),
  quantile(Q, 0.5, na.rm = TRUE),
  quantile(Q, 0.7, na.rm = TRUE),
  quantile(Q, 0.9, na.rm = TRUE),
  quantile(Q, 0.95, na.rm = TRUE)
), by = Site]
names(Stream_map) <- c("Site", "Mean", "Sd", paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95)))

map_site <- Siteinfo[, c("site_no", "dec_lat_va", "dec_lon_va")]
names(map_site) <- c("Site", "lat", "long")

Stream_map_stat <- full_join(Stream_map, map_site, by = c("Site" = "Site"))

Stream_map_stat[,mean_cen:=Mean-mean(Mean, na.rm = TRUE)]

############### map ################

pal <- colorNumeric(
  palette = colorRampPalette(c('green', 'red'))(length(Stream_map_stat$q0.05)), 
  domain = Stream_map_stat$q0.05)

Stream_map_stat %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=~long,
                   lat=~lat,
                   color = ~pal(q0.05), 
                   radius = ~5,
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 0.4)%>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~q0.05,
            title = "5% quantile",
            labFormat = labelFormat(),
            opacity = 1)%>%
  addProviderTiles("Esri.NatGeoWorldMap")%>%
  addGraticule(interval = 20)

# ggplot

plot(density(Stream_map_stat$Mean, na.rm=TRUE))

Stream_map_stat%>%
ggplot(aes(long, lat)) +
  geom_point(aes(color = q0.05, size = 7, alpha = 0.7)) +
  scale_colour_gradientn(colours = c("blue","green","yellow","orange","red")) +
  borders("state")
?scale_colour_gradientn
ggplot(Stream_map_stat, aes(long, lat)) +
  geom_point(aes(color = q0.1), size = 7, alpha = 0.5) +
  scale_colour_gradient(low = "green", high = "red") +
  borders("state")

ggplot(Stream_map_stat, aes(long, lat)) +
  geom_point(aes(color = q0.2), size = 7, alpha = 0.5) +
  scale_colour_gradient(low = "green", high = "red") +
  borders("state")

ggplot(Stream_map_stat, aes(long, lat)) +
  geom_point(aes(color = q0.5), size = 7, alpha = 0.5) +
  scale_colour_gradient(low = "green", high = "red") +
  borders("state")

ggplot(Stream_map_stat, aes(long, lat)) +
  geom_point(aes(color = q0.7), size = 7, alpha = 0.5) +
  scale_colour_gradient(low = "green", high = "red") +
  borders("state")

ggplot(Stream_map_stat, aes(long, lat)) +
  geom_point(aes(color = q0.9), size = 7, alpha = 0.5) +
  scale_colour_gradient(low = "green", high = "red") +
  borders("state")

ggplot(Stream_map_stat, aes(long, lat)) +
  geom_point(aes(color = scale(q0.95)), size = 7, alpha = 0.5) +
  scale_colour_gradient(low = "white", high = "red") +
  borders("state")

############################### statistics year ##########################

Stream_map_year <- Stream[, .(
  mean(Q), sd(Q),
  quantile(Q, 0.05, na.rm = TRUE),
  quantile(Q, 0.1, na.rm = TRUE),
  quantile(Q, 0.2, na.rm = TRUE),
  quantile(Q, 0.5, na.rm = TRUE),
  quantile(Q, 0.7, na.rm = TRUE),
  quantile(Q, 0.9, na.rm = TRUE),
  quantile(Q, 0.95, na.rm = TRUE)
), by = .(Site, DecYear)]
names(Stream_map_year) <- c("Site", "Year", "Mean", "Sd", paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95)))

Stream_map_year_stat <- full_join(Stream_map_year, map_site, by = c("Site" = "Site"))

setkey(Stream_map_year_stat,Year)
ggplot(Stream_map_year_stat[Year==1980], aes(long, lat)) +
  geom_point(aes(color = q0.05), size = 7, alpha = 0.5) +
  scale_colour_gradient(low = "green", high = "red") +
  borders("state")# +
#  facet_wrap(~Year)

############################### Rridgeline ####################################
Stream_plot[,Region:=as.factor(Region)][
  ,q0.999:= quantile(Q,0.999,na.rm = TRUE),by=Region][
    Q>q0.999
  ] %>%
  ggplot(aes(x=Q, y=Region, fill=Region)) +
  geom_density_ridges()+
  theme_ridges()





