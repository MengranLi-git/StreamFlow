library(data.table)
library(tidyverse)
library(ismev)
library(evd)

setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]
Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]
index <- Region_name[-(19:21), 2]
for(i in 1:18){
  assign(paste0("Region",i), Stream_plot[Region == index[i]][,Q])
}

setwd("F:/StreaFlow/Plot/MeanResidualLife")
par(mfrow=c(3,6))
for(i in 1:18){
#  png(paste0("Region",i,".png"))
  mrlplot(get(paste0("Region",1)),sub=paste0("Region",1))
}
dev.off()

data = Region5
tcplot(data,c(500,1000))

length(data[data>700])

fit1 <- gpd.fit(data, threshold=1000)
fit2 <- gpd.fit(data, threshold=500)
fit3 <- gpd.fit(data, threshold=800)
fit4 <- gpd.fit(data, threshold=300)
fit5 <- gpd.fit(data, threshold=600)

fit1$nllh
fit2$nllh
# s
fit2 <- gpd.fit(data, threshold=600)
# single
fit3 <- gpd.fit(data, threshold=600,ydat=matrix(1:length(data)),sigl = 1)

2*(fit2$nllh-fit3$nllh)
qchisq(0.95,1)