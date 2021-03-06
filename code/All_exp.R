setwd("F:/Streamflow/code/")
source("CI.R")
source("cut_breakpoint.R")
source("Moving_window.R")
source("ll.R")
source("graph_data.R")
source("plot_para.R")
source("fit_exp.R")

library(data.table)
library(tidyverse)
library(ismev)
library(gridExtra)

#### read data ####
setwd("F:\\StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]
Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]
index <- Region_name[-(19:21),2]

#### define breakpoint ####
point <- data.frame(mu=c(16,20,20,30,20,20,16,20,16,20,16,20,20,16,20,25,27,20),
                    sig=c(20,30,25,20,20,27,16,20,20,20,20,20,16,16,25,26,25,20),
                    sh=c(15,30,22,20,14,18,25,25,20,16,20,20,20,20,25,25,25,25))

gra <- list()
for(i in 1:18){
  i=18
  assign("Region",Stream_plot[Region==index[i]][,max(Q),by=DecYear])
  #MOV
  MOV <- Moving_window(x=Region, win = 10, url = "F:/StreamFlow/Plot/Moving_Windows", 
                       save = FALSE, name = quote(Region1))
  x <- Region$V1
  fit1 <- fit_exp(x,point[i,1],point[i,2],point[i,3])
  mu=point[i,1]
  sig=point[i,2]
  sh=point[i,3]
  est <- graph_data(fit1,name=c("best_fit","s_fit","single_fit","double_fit"))
  gra[[i]] <- plot_para(est[[1]],est[[2]],est[[3]],
                        Region,i)
  # ignore = c("double_fit","single_fit"))
}
#### arrange plot ####
setwd("F:/StreamFlow/Report")
plot_final <- list()
for(i in 1:18){
  p <- gra[[i]]
  plot_final[[3*(i-1)+1]] <- p[[1]]
  plot_final[[3*(i-1)+2]] <- p[[2]]
  plot_final[[3*(i-1)+3]] <- p[[3]]
}
plot_final[["ncol"]] = 3
plot_final[["nrow"]] = 18
plot_final[["common.legend"]] <- TRUE
save(plot_final,file="exp.Rdata")
do.call(grid.arrange,plot_final)

plot_final <- list()
for(i in 7:12){
  p <- gra[[i]]
  plot_final[[3*(i-7)+1]] <- p[[1]]
  plot_final[[3*(i-7)+2]] <- p[[2]]
  plot_final[[3*(i-7)+3]] <- p[[3]]
}
plot_final[["ncol"]] = 3
plot_final[["nrow"]] = 6

do.call(grid.arrange,plot_final)

plot_final <- list()
for(i in 13:18){
  p <- gra[[i]]
  plot_final[[3*(i-13)+1]] <- p[[1]]
  plot_final[[3*(i-13)+2]] <- p[[2]]
  plot_final[[3*(i-13)+3]] <- p[[3]]
}
plot_final[["ncol"]] = 3
plot_final[["nrow"]] = 6

do.call(grid.arrange,plot_final)







