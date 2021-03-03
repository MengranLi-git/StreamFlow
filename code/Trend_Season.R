library(data.table)
library(tidyverse)
library(KernSmooth)
library(leaflet)
library(maps)

setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]

Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]
setkey(Stream_plot, Region)

Stream_plot[Stream_plot[, Month %in% 3:5], Season := "Spring"]
Stream_plot[Stream_plot[, Month %in% 6:8], Season := "Summer"]
Stream_plot[Stream_plot[, Month %in% 9:11], Season := "Autumn"]
Stream_plot[Stream_plot[, Month %in% c(1, 2, 12)], Season := "Winter"]

Stream_Series <- Stream_plot[, mean(Q, na.rm = TRUE),
  by = .(DecYear, Season)
]
Stream_Series %>%
  ggplot(aes(x = DecYear, y = V1, group = Season, colour = Season)) +
  geom_line() +
  coord_fixed(ratio = 1)

acf_Data <- spread(Stream_Series, key = "Season", value = "V1")
acf_Data <- ts(acf_Data[, -1])
acf(acf_Data[, 1], main = "Spring")
acf(acf_Data[, 2], main = "Summer")
acf(acf_Data[, 3], main = "Autumn")
acf(acf_Data[, 4], main = "Winter")

acf(diff(acf_Data[, 1]), main = "Spring")
acf(diff(acf_Data[, 3]), main = "Autumn")
acf(diff(acf_Data[, 4]), main = "Winter")

Seasonname <- c("Spring", "Summer", "Autumn", "Winter")
Stream_Series <- Stream_plot[, mean(Q, na.rm = TRUE),
  by = .(DecYear, Season, Region)
]
H <- as.matrix(Stream_Series[, dpill(DecYear, V1), by = .(Season, Region)])

Regionname <- levels(as.factor(Stream_Series$Region))
Stream_Series_fit <- list()
for (i in 1:18) {
  H_1 <- subset(as.data.frame(H), Season == "Spring")
  data <- Stream_Series[Region == Regionname[i] & Season == "Spring"]
  Year <- as.numeric(as.vector(data$DecYear))
  Q <- data$V1
  Stream_Series_fit[[i]] <- locpoly(Year, Q, bandwidth = as.numeric(H_1[i, 3]))
  Stream_Series_fit[[i]]$Region <- Regionname[i]
}
Stream_Series_fit1 <- rbindlist(Stream_Series_fit)
Stream_Series_fit1$Season <- "Spring"

Stream_Series_fit <- list()
for (i in 1:18) {
  H_1 <- subset(as.data.frame(H), Season == "Summer")
  data <- Stream_Series[Region == Regionname[i] & Season == "Summer"]
  Year <- as.numeric(as.vector(data$DecYear))
  Q <- data$V1
  Stream_Series_fit[[i]] <- locpoly(Year, Q, bandwidth = as.numeric(H_1[i, 3]))
  Stream_Series_fit[[i]]$Region <- Regionname[i]
}
Stream_Series_fit2 <- rbindlist(Stream_Series_fit)
Stream_Series_fit2$Season <- "Summer"

Stream_Series_fit <- list()
for (i in 1:18) {
  H_1 <- subset(as.data.frame(H), Season == "Autumn")
  data <- Stream_Series[Region == Regionname[i] & Season == "Autumn"]
  Year <- as.numeric(as.vector(data$DecYear))
  Q <- data$V1
  Stream_Series_fit[[i]] <- locpoly(Year, Q, bandwidth = as.numeric(H_1[i, 3]))
  Stream_Series_fit[[i]]$Region <- Regionname[i]
}
Stream_Series_fit3 <- rbindlist(Stream_Series_fit)
Stream_Series_fit3$Season <- "Autumn"

Stream_Series_fit <- list()
for (i in 1:18) {
  H_1 <- subset(as.data.frame(H), Season == "Winter")
  data <- Stream_Series[Region == Regionname[i] & Season == "Winter"]
  Year <- as.numeric(as.vector(data$DecYear))
  Q <- data$V1
  Stream_Series_fit[[i]] <- locpoly(Year, Q, bandwidth = as.numeric(H_1[i, 3]))
  Stream_Series_fit[[i]]$Region <- Regionname[i]
}
Stream_Series_fit4 <- rbindlist(Stream_Series_fit)
Stream_Series_fit4$Season <- "Winter"

Season_plot <- rbind(Stream_Series_fit1, Stream_Series_fit2)
Season_plot <- rbind(Season_plot, Stream_Series_fit3)
Season_plot <- rbind(Season_plot, Stream_Series_fit4)

Season_plot %>%
  ggplot(aes(x = x, y = y, group = Season, colour = Season)) +
  geom_line() +
  coord_fixed(ratio = 0.5) +
  facet_wrap(~Region)

Stream_summary <- Stream[, .(
  mean(Q, na.rm = TRUE),
  sd(Q, na.rm = TRUE),
  quantile(Q, 0.05, na.rm = TRUE),
  quantile(Q, 0.1, na.rm = TRUE),
  quantile(Q, 0.2, na.rm = TRUE),
  quantile(Q, 0.5, na.rm = TRUE),
  quantile(Q, 0.7, na.rm = TRUE),
  quantile(Q, 0.9, na.rm = TRUE),
  quantile(Q, 0.95, na.rm = TRUE)
), by = Site]
names(Stream_summary) <- c(
  "Site", "Mean", "Sd",
  paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95))
)
Stream_summary <- gather(Stream_summary, "Mean", "Sd",
  paste0("q", c(0.05, 0.1, 0.2, 0.5, 0.7, 0.9, 0.95)),
  key = "statistics", value = "Q"
)

map_site <- Siteinfo[, c("site_no", "dec_lat_va", "dec_lon_va")]
names(map_site) <- c("Site", "lat", "long")

Stream_summary <- full_join(Stream_summary, map_site, by = c("Site" = "Site"))

Stream_summary <- as.data.table(Stream_summary)
summary_stat <- Stream_summary[, summary(Q), by = statistics]

summary_stat[, stat := rep(
  c("min", "Q1", "Q2", "Mean", "Q3", "max"), 9
)] %>%
  spread(key = "statistics", value = "V1")

Stream_summary %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = Q, size = I(0.5), alpha = I(0.3))) +
  scale_colour_gradientn(colours = c("blue", "green", "yellow", "orange", "red")) +
  borders("state", size = I(0.2)) +
  coord_fixed(ratio = 1.5) +
  facet_wrap(~statistics)
