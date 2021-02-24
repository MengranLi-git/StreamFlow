############################## library package ################################
library(data.table)
library(tidyverse)
library(KernSmooth)
library(leaflet)
library(maps)
############################### read data #####################################
setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]
setkey(Stream_plot, Region)

############### boxplot (finish)##############

# Boxplot of population

Stream_plot[, boxplot(Q ~ DecYear)]

# run more slowly
p <- ggplot(Stream_plot, aes(x = as.factor(DecYear), y = Q)) +
       geom_boxplot()

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
rm(data, H, i, Q)

rbindlist(Stream_Series_fit)[
  ,Month:=factor(Month, labels = month.abb)] %>%
  ggplot(aes(x=x,y=y,group=Month,colour=Month))+
  geom_line()+
  labs(x = "Year", y = "Streamflow", title = "Trend of Monthly Streamflow")
############################### statistics ##########################
Stream_summary <-  Stream[, .(
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
  quantile(Q, 0.999, na.rm = TRUE))]
names(Stream_summary) <- c("Mean", "Sd",
                       paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95, 0.975, 0.999)))

############################# Map of statistics ################################
map_site <- Siteinfo[, c("site_no", "dec_lat_va", "dec_lon_va")]
names(map_site) <- c("Site", "lat", "long")

Stream_map_full <- full_join(Stream_plot, map_site, by = c("Site" = "Site"))

############### Right tail ################
u <- Stream_summary[,q0.9]
Stream_map_full[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=Site]%>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = 7, alpha = 0.7)) +
  scale_colour_gradientn(colours = c("blue","green","yellow","orange","red")) +
  labs(title = "Mean Exceedance of 90% quantile")+
  borders("state")

u <- Stream_summary[,q0.95]
Stream_map_full[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=Site]%>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = 7, alpha = 0.7)) +
  scale_colour_gradientn(colours = c("blue","green","yellow","orange","red")) +
  labs(title = "Mean Exceedance of 95% quantile")+
  borders("state")

u <- Stream_summary[,q0.975]
Stream_map_full[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=Site]%>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = 7, alpha = 0.7)) +
  scale_colour_gradientn(colours = c("blue","green","yellow","orange","red")) +
  labs(title = "Mean Exceedance of 97.5% quantile")+
  borders("state")

u <- Stream_summary[,q0.999]
Stream_map_full[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=.(Site, DecYear)]%>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = 7, alpha = 0.7)) +
  scale_colour_gradientn(colours = c("blue","green","yellow","orange","red")) +
  labs(title = "Mean Exceedance of 99.9% quantile")+
  borders("state")+
  facet_wrap(~ DecYear)
############### Left tail #######################
u <- Stream_summary[,q0.1]
Stream_map_full[Q < u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=Site]%>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = 7, alpha = 0.7)) +
  scale_colour_gradientn(colours = c("blue","green","yellow","orange","red")) +
  labs(title = "Mean Exceedance of 10% quantile")+
  borders("state")

u <- Stream_summary[,q0.05]
Stream_map_full[Q < u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=Site]%>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = 7, alpha = 0.7)) +
  scale_colour_gradientn(colours = c("blue","green","yellow","orange","red")) +
  labs(title = "Mean Exceedance of 5% quantile")+
  borders("state")

############################# SPA AND TEM ######################################
Stream_summary_year <-  Stream[, .(
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
  quantile(Q, 0.99, na.rm = TRUE)), by = DecYear]
names(Stream_summary_year) <- c("DecYear", "Mean", "Sd",
                           paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)))

u <- Stream_summary_year[,c("DecYear", "q0.7")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=.(Site, DecYear)] %>% 
  ggplot(aes(long, lat)) + 
  geom_point(mapping=aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) + 
  labs(title = 'Mean Exceedance of 70% Quantile over 1980-2019',
       subtitle = 'Year:{current_frame}', 
       color = 'Mean Exceedance',
       y = "Lat",
       x = "Long") + 
  scale_colour_gradientn(colours = c("purple", "skyblue","green","yellow","orange","red")) +
  borders("state")+
  coord_fixed(ratio=1.5)+
  transition_manual(DecYear) + 
  ease_aes('linear')
animate(P, fps=2.5)

u <- Stream_summary_year[,c("DecYear", "q0.9")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=.(Site, DecYear)] %>% 
  ggplot(aes(long, lat)) + 
  geom_point(mapping=aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) + 
  labs(title = 'Mean Exceedance of 90% Quantile over 1980-2019',
       subtitle = 'Year:{current_frame}', 
       color = 'Mean Exceedance',
       y = "Lat",
       x = "Long") + 
  scale_colour_gradientn(colours = c("purple", "skyblue","green","yellow","orange","red")) +
  borders("state")+
  coord_fixed(ratio=1.5)+
  transition_manual(DecYear) + 
  ease_aes('linear')
animate(P, fps=2.5)

u <- Stream_summary_year[,c("DecYear", "q0.95")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=.(Site, DecYear)] %>% 
  ggplot(aes(long, lat)) + 
  geom_point(mapping=aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) + 
  labs(title = 'Mean Exceedance of 95% Quantile over 1980-2019',
       subtitle = 'Year:{current_frame}', 
       color = 'Mean Exceedance',
       y = "Lat",
       x = "Long") + 
  scale_colour_gradientn(colours = c("purple", "skyblue","green","yellow","orange","red")) +
  borders("state")+
  coord_fixed(ratio=1.5)+
  transition_manual(DecYear) + 
  ease_aes('linear')
animate(P, fps=2.5)

u <- Stream_summary_year[,c("DecYear", "q0.975")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=.(Site, DecYear)] %>% 
  ggplot(aes(long, lat)) + 
  geom_point(mapping=aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) + 
  labs(title = 'Mean Exceedance of 97.5% Quantile over 1980-2019',
       subtitle = 'Year:{current_frame}', 
       color = 'Mean Exceedance',
       y = "Lat",
       x = "Long") + 
  scale_colour_gradientn(colours = c("purple", "skyblue","green","yellow","orange","red")) +
  borders("state")+
  coord_fixed(ratio=1.5)+
  transition_time(DecYear) + 
  ease_aes('linear')
animate(P, fps=2.5)

u <- Stream_summary_year[,c("DecYear", "q0.99")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=.(Site, DecYear)] %>% 
  ggplot(aes(long, lat)) + 
  geom_point(mapping=aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) + 
  labs(title = 'Mean Exceedance of 99% Quantile over 1980-2019',
       subtitle = 'Year:{current_frame}', 
       color = 'Mean Exceedance',
       y = "Lat",
       x = "Long") + 
  scale_colour_gradientn(colours = c("purple", "skyblue","green","yellow","orange","red")) +
  borders("state")+
  coord_fixed(ratio=1.5)+
  transition_time(DecYear) + 
  ease_aes('linear')
animate(P, fps=2.5)

u <- Stream_summary_year[,c("DecYear", "q0.1")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q < u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=.(Site, DecYear)] %>% 
  ggplot(aes(long, lat)) + 
  geom_point(mapping=aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) + 
  labs(title = 'Mean Exceedance of 10% Quantile over 1980-2019',
       subtitle = 'Year:{current_frame}', 
       color = 'Mean Exceedance',
       y = "Lat",
       x = "Long") + 
  scale_colour_gradientn(colours = c("purple", "skyblue","green","yellow","orange","red")) +
  borders("state")+
  coord_fixed(ratio=1.5)+
  transition_manual(DecYear) + 
  ease_aes('linear')
animate(P, fps=2.5)

u <- Stream_summary_year[,c("DecYear", "q0.05")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q < u][
  , MeanExceedance:=mean(Q-u, na.rm = TRUE), by=.(Site, DecYear)] %>% 
  ggplot(aes(long, lat)) + 
  geom_point(mapping=aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) + 
  labs(title = 'Mean Exceedance of 5% Quantile over 1980-2019',
       subtitle = 'Year:{current_frame}', 
       color = 'Mean Exceedance',
       y = "Lat",
       x = "Long") + 
  scale_colour_gradientn(colours = c("purple", "skyblue","green","yellow","orange","red")) +
  borders("state")+
  coord_fixed(ratio=1.5)+
  transition_time(DecYear) + 
  ease_aes('linear')
animate(P, fps=2.5)
################### WASTE #########################
{Stream_map <- Stream[, .(
  mean(Q), sd(Q),
  quantile(Q, 0.05, na.rm = TRUE),
  quantile(Q, 0.1, na.rm = TRUE),
  quantile(Q, 0.2, na.rm = TRUE),
  quantile(Q, 0.5, na.rm = TRUE),
  quantile(Q, 0.7, na.rm = TRUE),
  quantile(Q, 0.9, na.rm = TRUE),
  quantile(Q, 0.95, na.rm = TRUE),
  quantile(Q, 0.975, na.rm = TRUE),
  quantile(Q, 0.999, na.rm = TRUE)
), by = Site]
names(Stream_map) <- c("Site", "Mean", "Sd",
                       paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95, 0.975, 0.999)))

Stream_map_stat <- full_join(Stream_map, map_site, by = c("Site" = "Site"))

summary(Stream_map_stat[,-1])

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
}
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





