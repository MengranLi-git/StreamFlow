library(data.table)
library(tidyverse)
library(ismev)
setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]

Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]
setkey(Stream_plot, Region)

para_t <- function(X){
  X <- X[,max(Q),by=DecYear]
  parameter <- as.data.frame(matrix(NA, ncol=3, nrow=25))
  for (i in 1:25){
    fit1 <- gev.fit(X$V1[i:(i+15)])
    parameter[i,] <- fit1$mle
  }
  return(parameter)
}

Stream_plot <- Stream_plot[,Region:=as.factor(Region)]

paralist <- by(Stream_plot, Stream_plot$Region, para_t)

setwd("F:/StreamFlow/Plot/Moving_Windows")
for (i in 1:18){
  X <- paralist[[i]]
  ggplot(X, aes(x=order(row.names(X)), y=V1))+
    geom_point()+
    xlab("t")+
    ylab("location")
  ggsave(paste0("location",i,".png"), plot=last_plot())
  ggplot(X, aes(x=order(row.names(X)), y=V2))+
    geom_point()+
    xlab("t")+
    ylab("scale")
  ggsave(paste0("scale",i,".png"), plot=last_plot())
}

## facet plot
paralist <- rbindlist(paralist)
paralist$Region <- rep(1:18,each=25)
paralist$t <- rep(1:25, 18)

ggplot(paralist, aes(x=t, y=V1))+
  geom_point()+
  xlab("t")+
  ylab("location")+
  facet_wrap(~Region)
