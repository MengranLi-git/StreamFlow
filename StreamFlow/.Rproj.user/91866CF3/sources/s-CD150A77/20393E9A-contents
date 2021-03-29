fit_quad <- function(x){
  single <- as.matrix(1:40)
  quad <- single^2
  model <- c("mu=quad,sig=s,sh=s",
             "mu=s,sig=quad,sh=s",
             "mu=s,sig=s,sh=quad",
             "mu=quad,sig=single,sh=s",
             "mu=quad,sig=s,sh=single",
             "mu=single,sig=quad,sh=s",
             "mu=s,sig=quad,sh=single",
             "mu=single,sig=s,sh=quad",
             "mu=s,sig=single,sh=quad",
             "mu=quad,sig=single,sh=single",
             "mu=single,sig=quad,sh=single",
             "mu=single,sig=single,sh=quad",
             "mu=quad,sig=quad,sh=s",
             "mu=quad,sig=s,sh=quad",
             "mu=s,sig=quad,sh=quad",
             "mu=quad,sig=quad,sh=single",
             "mu=quad,sig=single,sh=quad"
             # "mu=single,sig=quad,sh=quad",
  )
  # quad, s, s
  y <- cbind(single,quad)
  fit1 <- gev.fit(x,y, mul=c(1:2))
  fit1[["df"]] <- 5
  fit2 <- gev.fit(x,y, sigl=c(1:2))
  fit2[["df"]] <- 5
  fit3 <- gev.fit(x,y, shl=c(1:2))
  fit3[["df"]] <- 5
  # quad, s, single
  y <- cbind(single,quad)
  fit4 <- gev.fit(x,y, mul=c(1:2),sigl = 1)
  fit4[["df"]] <- 6
  fit5 <- gev.fit(x,y, mul=c(1:2),shl = 1)
  fit5[["df"]] <- 6
  fit6 <- gev.fit(x,y, mul=1, sigl=c(1:2))
  fit6[["df"]] <- 6
  fit7 <- gev.fit(x,y, sigl=c(1:2), shl=1)
  fit7[["df"]] <- 6
  fit8 <- gev.fit(x,y, mul=1,shl=c(1:2))
  fit8[["df"]] <- 6
  fit9 <- gev.fit(x,y, sigl=1,shl=c(1:2))
  fit9[["df"]] <- 6
  
  # quad, single, single
  fit10 <- gev.fit(x,y, mul=c(1:2),sigl = 1, shl=1)
  fit10[["df"]] <- 7
  fit11 <- gev.fit(x,y, mul=1,sigl = c(1:2), shl=1)
  fit11[["df"]] <- 7
  fit12 <- gev.fit(x,y, mul=1,sigl = 1, shl=c(1:2))
  fit12[["df"]] <- 7
  # quad, quad, s
  fit13 <- gev.fit(x,y, mul=c(1:2),sigl = c(1:2), shl=1)
  fit13[["df"]] <- 8
  fit14 <- gev.fit(x,y, mul=c(1:2),sigl = 1, shl=c(1:2))
  fit14[["df"]] <- 8
  fit15 <- gev.fit(x,y, mul=1,sigl = c(1:2), shl=c(1:2))
  fit15[["df"]] <- 8
  # quad, quad, single
  fit16 <- gev.fit(x,y, mul=c(1:2),sigl = c(1:2), shl=1)
  fit16[["df"]] <- 8
  fit17 <- gev.fit(x,y, mul=c(1:2),sigl = 1, shl=c(1:2))
  fit17[["df"]] <- 8
  fit18 <- gev.fit(x,y, mul=1,sigl = c(1:2), shl=c(1:2))
  fit18[["df"]] <- 8
  # quad, quad, quad
  # fit19 <- gev.fit(x,y, mul=c(1:2),sigl = c(1:2), shl=c(1:2))
  
  AIC <- numeric(18)
  for(i in 1:18){
    AIC[i] <- get(paste0("fit",i))$nllh+get(paste0("fit",i))$df
  }  
  fit <- get(paste0("fit", which.min(AIC)))
  fit[["which is quad"]] <- model[which.min(AIC)]
  return(fit)
}
