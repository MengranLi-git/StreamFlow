library(data.table)
library(tidyverse)
library(ismev)
setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]

Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]

X <- Stream_plot[Region=="01"][,max(Q),by=DecYear]
X$t <- 1:40
point_mu_1 <- 17
#point_mu_2 <- 
point_sigma_1 <- 9
#point_sigma_2 <-

X$t_mu_1 <- X$t-point_mu_1
X$t_mu_2 <- X$t-point_mu_1
subset(X, t_mu_1>0)=0
X[which(X$t_mu_1>=0), "t_mu_1"] = 0
X[which(X$t_mu_2<0), "t_mu_2"] = 0
fit2 <- gev.fit(xdat = X$V1,
        ydat = as.matrix(X[,4:5]), mul=c(1,2))

y <- fit2$mle[1] + fit2$mle[2]*X[,4]+fit2$mle[3]*X[,5]

para <- para_t(Stream_plot[Region=="01"])

ggplot(y,aes(x=1:40,y=t_mu_1))+
  geom_line()+
  geom_point(data = para, aes(x=7+order(row.names(para)), y=V1, color=I("blue")))+
  geom_point(data = Region1, aes(x=DecYear-1980, y=V1,color="red"))+
  xlab("t")+
  ylab("location")

## ?Discontinuous
## affected by the extreme value
7+8=15

