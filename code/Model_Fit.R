library(data.table)
library(tidyverse)
library(ismev)
setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]
Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]

## Block

Region1 <- Stream_plot[Region=="01"][,max(Q),by=DecYear]

## Moving_window
x <- Region1
Moving_window <- function(x, win=15, url, save=TRUE, name){
  n_row = nrow(x)
  n_sep = round(win/2)
#  fit <- gev.fit(x$V1)
#  shape <- fit$mle[3]
  parameter <- as.data.frame(matrix(NA, ncol=4, nrow=n_row))
  names(parameter) <- c("t","location","scale","shape")
  for (i in 1:(n_row-2*n_sep)){
    fit <- gev.fit(x$V1[i:(i+win)])
    parameter[i+n_sep,1] <- i+n_sep
    parameter[i+n_sep,2:4] <- fit$mle
  }
  if(save == TRUE){
    ggplot(parameter, aes(x=t, y=location))+
      geom_point()+
      xlab("t")+
      ylab("location")+
    ggsave(paste0(url,"/","location",name,".png"), plot=last_plot(),height=3,width=4)
    ggplot(parameter, aes(x=t, y=scale))+
      geom_point()+
      xlab("t")+
      ylab("scale")
    ggsave(paste0(url,"/","scale",name,".png"), plot=last_plot(),height=3,width=4)
    ggplot(parameter, aes(x=t, y=shape))+
      geom_point()+
      xlab("t")+
      ylab("shape")
    ggsave(paste0(url,"/","shape",name,".png"), plot=last_plot(),height=3,width=4)
    
  }
  return(parameter)
}

## An exmaple
MOV <- Moving_window(x=Region1, win = 10, url = "F:/StreamFlow/Plot/Moving_Windows", 
                     save = FALSE, name = quote(Region1))

## All Region
for (i in 1:18){
  Region <- Stream_plot[Region==Region_name[i,"Hub"]][,max(Q),by=DecYear]
  Moving_window(x=Region, win = 10, url = "F:/StreamFlow/Plot/Moving_Windows", 
                save = TRUE, name = Region_name[i,"Hub"])
}


## Double_linear
Double_linear <- function(x, point_mu=0, point_sigma=0, S_mu = FALSE, S_sigma = FALSE){
  n_row = nrow(x)
  Double <- data.table(t=1:n_row)
  Double$V1 <- x$V1
  Double$D1 <- 0
  Double[(point_mu+1):n_row,"D1"] <- 1
  Double$mu_1 <- (Double$t-point_mu)*(1-Double$D1)
  Double$mu_2 <- (Double$t-point_mu)*Double$D1
  Double$D2 <- 0
  Double[(point_sigma+1):n_row,"D2"] <- 1
  Double$sigma_1 <- (Double$t-point_sigma)*(1-Double$D2)
  Double$sigma_2 <- (Double$t-point_sigma)*Double$D2
  if(S_mu){
    data1 = NULL
    mul = NULL}else{
      if(point_mu == 0){
        data1 = Double[,"mu_2"]
        mul = c(1)
      }else{
        data1=Double[,c("mu_1","mu_2")]
        mul = c(1,2)
      }
    }
  if(S_sigma){
    data2 = NULL
    sigl = NULL}else{
      if(point_sigma==0){
        data2=Double[,"sigma_2"]
        sigl <- c(length(mul)+1)
      }else{
        data2=Double[,c("sigma_1","sigma_2")]
        sigl <- c((length(mul)+1):(length(mul)+2))
      }
    }
  fit <- gev.fit(xdat = x$V1,
          ydat = as.matrix(cbind(data1, data2)), mul=mul, sigl=sigl)
  return(fit)
}

## station
fit1 <- Double_linear(Region1, S_mu = TRUE, S_sigma = TRUE)
## mu_trend, sigma_station
fit2 <- Double_linear(Region1, point_mu = 0, S_sigma = TRUE)
## mu_station, sigma_trend
fit3 <- Double_linear(Region1, point_sigma = 0, S_mu = TRUE)
## mu_trend, sigma_trend
fit4 <- Double_linear(Region1, point_mu = 0, point_sigma = 0)
## mu_twice, sigma_station
fit5 <- Double_linear(Region1, point_mu = 14, S_sigma = TRUE)
## mu_twice, sigma_trend
fit6 <- Double_linear(Region1, point_mu = 14, point_sigma=0)
## mu_station, sigma_twice
fit7 <- Double_linear(Region1, point_sigma=16, S_mu = TRUE)
## mu_trend, sigma_twice
fit8 <- Double_linear(Region1, point_mu = 0, point_sigma=16)
## mu_twice, sigma_twice
fit9 <- Double_linear(Region1, point_mu = 14, point_sigma=16)


result <- data.table(df=c(3,4,4,5,6,7,6,7,9))
for(i in 1:9){
  result[i, "nllh"] <- get(paste0("fit",i))$nllh
}
result <- transform(result, AIC=2*(df+nllh))
which.min(result$AIC)
mle <- get(paste0("fit",which.min(result$AIC)))$mle
se <- get(paste0("fit",which.min(result$AIC)))$se

t <- 1:40

ggplot(y,aes(x=t,y=mu_t))+
  geom_line()+
  geom_line(aes(x=t,y=upper),linetype=2)+
  geom_line(aes(x=t,y=lower),linetype=2)+
  geom_point(data = MOV, aes(x=t, y=location, color=I("blue")))+
  geom_point(data = Region1, aes(x=DecYear-1980, y=V1,color="red"))+
  xlab("t")+
  ylab(expression(mu(t)))







