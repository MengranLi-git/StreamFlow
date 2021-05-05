#### read all function ####
setwd("F:/Streamflow/code/")
source("Moving_window.R")
source("cut_breakpoint.R")
source("ll.R")
source("fit_abrupt.R")
source("fit_linear.R")
source("fit_quad.R")
source("CI.R")
source("graph_data.R")
source("plot_para.R")

library(data.table)
library(tidyverse)
library(ismev)
library(ggpubr)

#### read data ####
setwd("F:/StreamFlow")
load("Stream.Rdata")
Stream <- Stream[Q >= 0]
Stream_plot <- Stream[, c("Site", "Q", "DecYear", "Month", "Region")]
index <- Region_name[-(19:21), 2]

#### define breakpoint ####
point <- data.frame(
  mu = c(16, 20, 20, 30, 20, 20, 16, 20, 16, 20, 16, 20, 20, 16, 20, 25, 27, 20),
  sig = c(20, 30, 25, 20, 20, 27, 16, 20, 20, 20, 20, 20, 16, 16, 25, 26, 25, 20),
  sh = c(15, 30, 22, 20, 14, 18, 25, 25, 20, 16, 20, 20, 20, 20, 25, 25, 25, 25)
)

#### fit models and plot ####
gra <- list()
for (i in 1:18) {
  assign("Region", Stream_plot[Region == index[i]][, max(Q), by = DecYear])
  # MOV
  MOV <- Moving_window(
    x = Region, win = 10, url = "F:/StreamFlow/Plot/Moving_Windows",
    save = FALSE, name = quote(Region1)
  )
  
  x <- Region$V1
  fit1 <- fit_linear(x, point[i, 1], point[i, 2], point[i, 3])
  fit2 <- fit_quad(x)
  fit3 <- fit_abrupt(x, point[i, 1], point[i, 2], point[i, 3])

  fit_list <- list(fit1[[1]], fit1[[2]], fit1[[3]], fit1[[4]], fit2, fit3)
  r= which.min(c(fit1[[3]]$AIC,
                 fit1[[4]]$AIC,
                 fit2$AIC,
                 fit3$AIC))
    fit_list[[7]] <- fit_list[[r+2]]

  est <- graph_data(
    fit_list,
    c("linear_fit", "s_fit", "single_fit", "double_fit", "quad_fit", "abrupt_fit", "best_fit")
  )
  gra[[i]] <- plot_para(
    est[[1]], est[[2]], est[[3]],
    Region, i,
  ignore = c("linear_fit","s_fit", "single_fit", "double_fit", "quad_fit", "abrupt_fit"))
  print(paste("region",i))
}

#### arrange plot ####
plot_final <- list()
for (i in 1:6) {
  p <- gra[[i]]
  plot_final[[3 * (i - 1) + 1]] <- p[[1]]
  plot_final[[3 * (i - 1) + 2]] <- p[[2]]
  plot_final[[3 * (i - 1) + 3]] <- p[[3]]
}
plot_final[["ncol"]] <- 3
plot_final[["nrow"]] <- 6
plot_final[["legend"]] <- FALSE
#plot_final[["common.legend"]] <- TRUE

#save(plot_final,file = "best.Rdata")
do.call(ggarrange, plot_final)

save(gra,file="plot.Rdata")
