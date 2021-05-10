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
source("return_level.R")

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
return_plot <- list()
for (i in 1:18) {
  assign("Region", Stream_plot[Region == index[i]][, max(Q), by = DecYear])
  # MOV
  #  MOV <- Moving_window(
  #    x = Region, win = 10, url = "F:/StreamFlow/Plot/Moving_Windows",
  #    save = FALSE, name = quote(Region1)
  #  )

  x <- Region$V1
  fit1 <- fit_linear(x, point[i, 1], point[i, 2], point[i, 3])
  fit2 <- fit_quad(x)
  fit3 <- fit_abrupt(x, point[i, 1], point[i, 2], point[i, 3])

  fit_list <- list(fit1[[1]], fit1[[2]], fit1[[3]], fit1[[4]], fit2, fit3)
  r <- which.min(c(
    fit1[[2]]$AIC,
    fit1[[3]]$AIC,
    fit1[[4]]$AIC,
    fit2$AIC,
    fit3$AIC
  ))
  fit_list[[7]] <- fit_list[[r + 1]]

  nR2 <- return_level(fit_list[[7]], 1 / 2)
  nR10 <- return_level(fit_list[[7]], 1 / 10)
  nR100 <- return_level(fit_list[[7]], 1 / 100)

  R2 <- return_level(fit1[[2]], 1 / 2)
  R10 <- return_level(fit1[[2]], 1 / 10)
  R100 <- return_level(fit1[[2]], 1 / 100)

  p2 <- ggplot(nR2) +
    geom_line(aes(x = Year, y = est, color = "nsGEV", linetype = "model")) +
    geom_line(aes(x = Year, y = U, color = "nsGEV", linetype = "95% CI")) +
    geom_line(aes(x = Year, y = L, color = "nsGEV", linetype = "95% CI")) +
    geom_line(data = R2, aes(x = Year, y = est, color = "sGEV", linetype = "model")) +
    geom_line(data = R2, aes(x = Year, y = U, color = "sGEV", linetype = "95% CI")) +
    geom_line(data = R2, aes(x = Year, y = L, color = "sGEV", linetype = "95% CI")) +
    labs(title = paste0("Region", i, ": 2-year"), ylab = "Return level") +
    xlab(NULL)+
    ylab("Return level") +
    scale_linetype_manual(values = c(2, 1))

  p10 <- ggplot(nR10) +
    geom_line(aes(x = Year, y = est, color = "nsGEV", linetype = "model")) +
    geom_line(aes(x = Year, y = U, color = "nsGEV", linetype = "95% CI")) +
    geom_line(aes(x = Year, y = L, color = "nsGEV", linetype = "95% CI")) +
    geom_line(data = R10, aes(x = Year, y = est, color = "sGEV", linetype = "model")) +
    geom_line(data = R10, aes(x = Year, y = U, color = "sGEV", linetype = "95% CI")) +
    geom_line(data = R10, aes(x = Year, y = L, color = "sGEV", linetype = "95% CI")) +
    labs(title = paste0("Region", i, ": 10-year")) +
    xlab(NULL)+
    ylab(NULL)+
    scale_linetype_manual(values = c(2, 1))

  p100 <- ggplot(nR100) +
    geom_line(aes(x = Year, y = est, color = "nsGEV", linetype = "model")) +
    geom_line(aes(x = Year, y = U, color = "nsGEV", linetype = "95% CI")) +
    geom_line(aes(x = Year, y = L, color = "nsGEV", linetype = "95% CI")) +
    geom_line(data = R100, aes(x = Year, y = est, color = "sGEV", linetype = "model")) +
    geom_line(data = R100, aes(x = Year, y = U, color = "sGEV", linetype = "95% CI")) +
    geom_line(data = R100, aes(x = Year, y = L, color = "sGEV", linetype = "95% CI")) +
    labs(title = paste0("Region", i, ": 100-year"), ylab = "Return level") +
    xlab(NULL)+
    ylab(NULL)+
    scale_linetype_manual(values = c(2, 1))

  return_plot[[i]] <- list(p2, p10, p100)

  est <- graph_data(
    fit_list,
    c("linear_fit", "s_fit", "single_fit", "double_fit", "quad_fit", "abrupt_fit", "best_fit")
  )
  gra[[i]] <- plot_para(
    est[[1]], est[[2]], est[[3]],
    Region, i,
    ignore = c("linear_fit", "s_fit", "single_fit", "double_fit", "quad_fit", "abrupt_fit")
  )
  print(paste("region", i))
  print(round(fit_list[[7]]$mle, 2))
  print(fit_list[[7]]$formulation)
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
# plot_final[["common.legend"]] <- TRUE

# save(plot_final,file = "best.Rdata")
do.call(ggarrange, plot_final)

ggsave("first.png", plot = last_plot(), width = 6, height = 8)

plot_final <- list()
for (i in 13:18) {
  p <- return_plot[[i]]
  plot_final[[3 * (i - 1) + 1]] <- p[[1]]
  plot_final[[3 * (i - 1) + 2]] <- p[[2]]
  plot_final[[3 * (i - 1) + 3]] <- p[[3]]
}
plot_final[["ncol"]] <- 3
plot_final[["nrow"]] <- 6
plot_final[["common.legend"]] <- TRUE

# save(plot_final,file = "best.Rdata")
do.call(ggarrange, plot_final)

ggsave("return3.png", plot = last_plot(), width = 7, height = 9)
