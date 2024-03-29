library(data.table)
library(tidyverse)
library(evd)

setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]

Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Date", "Region")]

# check every day data
day <- Stream_plot[,.N, by=.(Region, Date)]
day <- spread(day, key=Region,value=N)
which(is.na(day))

# Month block
region <- levels(as.factor(Stream_plot$Region))
setwd("F:/StreamFlow/Plot/Chi_plot") 

for (i in 1:18){
  for (j in i:18){
    if (i != j){
      png(file = paste0(region[i],"-",region[j],".png"))
      ser1 <- Stream_plot[Region==region[i]][,max(Q),by=.(DecYear, Month)][["V1"]]
      ser2 <- Stream_plot[Region==region[j]][,max(Q),by=.(DecYear, Month)][["V1"]]
      par(mfrow = c(1,2))
      chiplot(cbind(ser1,ser2), main1 = paste0("Chi Plot ",region[i],"-",region[j]), main2 = paste0("Chi Bar Plot ",region[i],"-",region[j]))
      dev.off()
    }
  }
}

# daily
ser1 <- Stream_plot[Region=="10"][,max(Q),by=Date][["V1"]]
ser2 <- Stream_plot[Region=="11"][,max(Q),by=Date][["V1"]]

par(mfrow = c(1,2))
chiplot(cbind(ser1,ser2), main1 = "Chi Plot 10-11", main2 = "Chi Bar Plot 10-11")
