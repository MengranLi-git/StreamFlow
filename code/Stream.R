############################## library package ################################
library(data.table)
library(tidyverse)
library(KernSmooth)
library(leaflet)
library(maps)
library(gganimate)
############################### read data #####################################
setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]

Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]
setkey(Stream_plot, Region)

summary(Stream$Q)
############### boxplot (finish)##############

# Boxplot of population

Stream_plot[, boxplot(Q ~ DecYear)]

blank <- rep(" ", 4)
label <- c()
for (i in 1:8) {
  label <- c(label, 1980 + 5 * (i - 1), blank)
}
Stream_plot[, DecYear := as.factor(DecYear)] %>%
  ggplot(aes(x = DecYear, y = Q)) +
  geom_boxplot() +
  scale_x_discrete(labels = label, breaks = 1980:2019)

# Boxplot grouped by region
blank <- rep(" ", 9)
label <- c()
for (i in 1:4) {
  label <- c(label, 1980 + 10 * (i - 1), blank)
}
Stream_plot[, DecYear := as.factor(DecYear)] %>%
  ggplot(aes(x = DecYear, y = Q, colour = Region)) +
  geom_boxplot() +
  scale_x_discrete(labels = label, breaks = 1980:2019) +
  facet_wrap(~Region)

############### Smooth ########################
Stream_Series <- Stream_plot[, mean(Q, na.rm = TRUE),
  by = .(DecYear, Month)
]
Stream_Series %>% spread(key = "Month", value = "V1")
H <- as.matrix(Stream_Series[, dpill(DecYear, V1), by = Month])

Stream_Series_fit <- list()
for (i in 1:12) {
  data <- Stream_Series[Month == i]
  Year <- as.numeric(as.vector(data$DecYear))
  Q <- data$V1
  Stream_Series_fit[[i]] <- locpoly(Year, Q, bandwidth = H[i, 2])
  Stream_Series_fit[[i]]$Month <- i
}

rbindlist(Stream_Series_fit)[
  , Month := factor(Month, labels = month.abb)
] %>%
  ggplot(aes(x = x, y = y, group = Month, colour = Month)) +
  geom_line() +
  labs(x = "Year", y = "Streamflow", title = "Trend of Monthly Streamflow") +
  coord_fixed(ratio = 1)
############### Month Group####################

Season <- rbindlist(Stream_Series_fit)
Season[which(Season$Month %in% c(3:5, 8:10)), "Group"] <- "A"
Season[which(!Season$Month %in% c(3:5, 8:10)), "Group"] <- "B"
Season$Month <- factor(Season$Month, labels = month.abb)

Season %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(group = Month, colour = Month)) +
  labs(x = "Year", y = "Streamflow", title = "Grouped Trend of Monthly Streamflow") +
  facet_wrap(~Group) +
  coord_fixed(ratio = 1)

head(Stream_minus)

############## Adf test ################
Stream_Series_test <- spread(Stream_Series, key = "Month", value = "V1")
Stream_Series_test <- ts(Stream_Series_test[, -1], start = 1980)

par(mfrow = c(3, 4))
for (i in 1:12) {
  acf(Stream_Series_test[, i], main = paste0("Month", i))
}
rm(Stream_Series_test, Stream_Series, H, Q, Year)
############################### statistics ##########################
Stream_summary <- Stream[, .(
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
)]
names(Stream_summary) <- c(
  "Mean", "Sd",
  paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99))
)

############################# Map of statistics ################################
map_site <- Siteinfo[, c("site_no", "dec_lat_va", "dec_lon_va")]
names(map_site) <- c("Site", "lat", "long")

Stream_map_full <- full_join(Stream_plot, map_site, by = c("Site" = "Site"))

############### Right tail ################
u <- Stream_summary[, q0.7]
Stream_map_full[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = Site
] %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = I(3), alpha = I(0.5))) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  labs(title = "Mean Exceedance of 70% quantile") +
  borders("state") +
  coord_fixed(ratio = 1.5)

u <- Stream_summary[, q0.9]
Stream_map_full[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = Site
] %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = I(4), alpha = I(0.5))) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  labs(title = "Mean Exceedance of 90% quantile") +
  borders("state") +
  coord_fixed(ratio = 1.5)

u <- Stream_summary[, q0.95]
Stream_map_full[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = Site
] %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = I(4), alpha = I(0.5))) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  labs(title = "Mean Exceedance of 95% quantile") +
  borders("state") +
  coord_fixed(ratio = 1.5)

u <- Stream_summary[, q0.975]
Stream_map_full[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = Site
] %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = I(4), alpha = I(0.5))) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  labs(title = "Mean Exceedance of 97.5% quantile") +
  borders("state") +
  coord_fixed(ratio = 1.5)

u <- Stream_summary[, q0.99]
Stream_map_full[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = .(Site, DecYear)
] %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = I(4), alpha = I(0.5))) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  labs(title = "Mean Exceedance of 99% quantile") +
  borders("state") +
  coord_fixed(ratio = 1.5)

############### Left tail #######################
u <- Stream_summary[, q0.2]
Stream_map_full[Q < u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = Site
] %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = I(4), alpha = I(0.5))) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  labs(title = "Mean Exceedance of 20% quantile") +
  borders("state") +
  coord_fixed(ratio = 1.5)

u <- Stream_summary[, q0.1]
Stream_map_full[Q < u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = Site
] %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = I(4), alpha = I(0.5))) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  labs(title = "Mean Exceedance of 10% quantile") +
  borders("state") +
  coord_fixed(ratio = 1.5)

u <- Stream_summary[, q0.05]
Stream_map_full[Q < u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = Site
] %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = MeanExceedance, size = I(4), alpha = I(0.5))) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  labs(title = "Mean Exceedance of 5% quantile") +
  borders("state") +
  coord_fixed(ratio = 1.5)

############################# SPA AND TEM ######################################
Stream_summary_year <- Stream[, .(
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
), by = DecYear]
names(Stream_summary_year) <- c(
  "DecYear", "Mean", "Sd",
  paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99))
)

u <- Stream_summary_year[, c("DecYear", "q0.7")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = .(Site, DecYear)
] %>%
  ggplot(aes(long, lat)) +
  geom_point(mapping = aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) +
  labs(
    title = "Mean Exceedance of 70% Quantile over 1980-2019",
    subtitle = "Year:{current_frame}",
    color = "Mean Exceedance",
    y = "Lat",
    x = "Long"
  ) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  borders("state") +
  coord_fixed(ratio = 1.5) +
  transition_manual(DecYear) +
  ease_aes("linear")
animate(P, fps = 2.5)

u <- Stream_summary_year[, c("DecYear", "q0.9")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = .(Site, DecYear)
] %>%
  ggplot(aes(long, lat)) +
  geom_point(mapping = aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) +
  labs(
    title = "Mean Exceedance of 90% Quantile over 1980-2019",
    subtitle = "Year:{current_frame}",
    color = "Mean Exceedance",
    y = "Lat",
    x = "Long"
  ) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  borders("state") +
  coord_fixed(ratio = 1.5) +
  transition_manual(DecYear) +
  ease_aes("linear")
animate(P, fps = 2.5)

u <- Stream_summary_year[, c("DecYear", "q0.95")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = .(Site, DecYear)
] %>%
  ggplot(aes(long, lat)) +
  geom_point(mapping = aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) +
  labs(
    title = "Mean Exceedance of 95% Quantile over 1980-2019",
    subtitle = "Year:{current_frame}",
    color = "Mean Exceedance",
    y = "Lat",
    x = "Long"
  ) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  borders("state") +
  coord_fixed(ratio = 1.5) +
  transition_manual(DecYear) +
  ease_aes("linear")
animate(P, fps = 2.5)

u <- Stream_summary_year[, c("DecYear", "q0.975")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = .(Site, DecYear)
] %>%
  ggplot(aes(long, lat)) +
  geom_point(mapping = aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) +
  labs(
    title = "Mean Exceedance of 97.5% Quantile over 1980-2019",
    subtitle = "Year:{current_frame}",
    color = "Mean Exceedance",
    y = "Lat",
    x = "Long"
  ) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  borders("state") +
  coord_fixed(ratio = 1.5) +
  transition_time(DecYear) +
  ease_aes("linear")
animate(P, fps = 2.5)

u <- Stream_summary_year[, c("DecYear", "q0.99")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q > u][
  , MeanExceedance := mean(Q - u, na.rm = TRUE),
  by = .(Site, DecYear)
] %>%
  ggplot(aes(long, lat)) +
  geom_point(mapping = aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) +
  labs(
    title = "Mean Exceedance of 99% Quantile over 1980-2019",
    subtitle = "Year:{current_frame}",
    color = "Mean Exceedance",
    y = "Lat",
    x = "Long"
  ) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  borders("state") +
  coord_fixed(ratio = 1.5) +
  transition_time(DecYear) +
  ease_aes("linear")
animate(P, fps = 2.5)

u <- Stream_summary_year[, c("DecYear", "q0.2")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
Plot_matrix <- Stream_map_Year[Q < u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = .(Site, DecYear)
]
P <- Plot_matrix %>%
  ggplot(aes(long, lat)) +
  geom_point(mapping = aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) +
  labs(
    title = "Mean Exceedance of 20% Quantile over 1980-2019",
    subtitle = "Year:{current_frame}",
    color = "Mean Exceedance",
    y = "Lat",
    x = "Long"
  ) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  borders("state") +
  coord_fixed(ratio = 1.5) +
  transition_manual(DecYear) +
  ease_aes("linear")
animate(P, fps = 2.5)

u <- Stream_summary_year[, c("DecYear", "q0.1")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q < u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = .(Site, DecYear)
] %>%
  ggplot(aes(long, lat)) +
  geom_point(mapping = aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) +
  labs(
    title = "Mean Exceedance of 10% Quantile over 1980-2019",
    subtitle = "Year:{current_frame}",
    color = "Mean Exceedance",
    y = "Lat",
    x = "Long"
  ) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  borders("state") +
  coord_fixed(ratio = 1.5) +
  transition_time(DecYear) +
  ease_aes("linear")
animate(P, fps = 2.5)

u <- Stream_summary_year[, c("DecYear", "q0.05")]
names(u)[2] <- "u"
Stream_map_Year <- full_join(Stream_map_full, u, by = c("DecYear" = "DecYear"))
P <- Stream_map_Year[Q < u][
  , MeanExceedance := mean(Q, na.rm = TRUE),
  by = .(Site, DecYear)
] %>%
  ggplot(aes(long, lat)) +
  geom_point(mapping = aes(color = MeanExceedance, alpha = I(0.5), size = I(3))) +
  labs(
    title = "Mean Exceedance of 5% Quantile over 1980-2019",
    subtitle = "Year:{current_frame}",
    color = "Mean Exceedance",
    y = "Lat",
    x = "Long"
  ) +
  scale_colour_gradientn(colours = c("purple", "skyblue", "green", "yellow", "orange", "red")) +
  borders("state") +
  coord_fixed(ratio = 1.5) +
  transition_time(DecYear) +
  ease_aes("linear")
animate(P, fps = 2.5)

############### map (waste temporarily) ################

pal <- colorNumeric(
  palette = colorRampPalette(c("green", "red"))(length(Stream_map_stat$q0.05)),
  domain = Stream_map_stat$q0.05
)

Stream_map_stat %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~long,
    lat = ~lat,
    color = ~ pal(q0.05),
    radius = ~5,
    stroke = FALSE,
    fill = TRUE,
    fillOpacity = 0.4
  ) %>%
  addLegend("bottomright",
    pal = pal,
    values = ~q0.05,
    title = "5% quantile",
    labFormat = labelFormat(),
    opacity = 1
  ) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addGraticule(interval = 20)

# ggplot

plot(density(Stream_map_stat$Mean, na.rm = TRUE))

Stream_map_stat %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = q0.05, size = 7, alpha = 0.7)) +
  scale_colour_gradientn(colours = c("blue", "green", "yellow", "orange", "red")) +
  borders("state")

############################### Rridgeline (waste temporarily) ####################################
Stream_plot[, Region := as.factor(Region)][
  , q0.99 := quantile(Q, 0.99, na.rm = TRUE),
  by = Region
][
  Q > q0.99
] %>%
  ggplot(aes(x = Q, y = Region, fill = Region)) +
  geom_density_ridges() +
  theme_ridges()
