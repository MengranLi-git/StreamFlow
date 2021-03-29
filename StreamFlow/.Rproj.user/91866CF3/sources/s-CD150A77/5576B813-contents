fit_double <- function(x,mu,sig,sh){
  single <- cut_breakpoint(1:40, breakpoint=0)
  mul <- cut_breakpoint(1:40, mu)
  sigl <- cut_breakpoint(1:40, sig)
  shl <- cut_breakpoint(1:40, sh)
  model <- c("mu=double,sig=s,sh=s",
             "mu=s,sig=double,sh=s",
             "mu=s,sig=s,sh=double",
             #45
             "mu=double,sig=single,sh=s",
             "mu=double,sig=s,sh=single",
             #67
             "mu=single,sig=double,sh=s",
             "mu=s,sig=double,sh=single",
             #89
             "mu=single,sig=s,sh=double",
             "mu=s,sig=single,sh=double",
             
             #10 11 12
             "mu=double,sig=single,sh=single",
             "mu=single,sig=double,sh=single",
             "mu=single,sig=single,sh=double",
             #13 14 15
             "mu=double,sig=double,sh=s",
             "mu=double,sig=s,sh=double",
             "mu=s,sig=double,sh=double",
             #16 17 18
             "mu=double,sig=double,sh=single",
             "mu=double,sig=single,sh=double",
             "mu=single,sig=double,sh=double",
             # 19
             "mu=double,sig=double,sh=double")
  # double, s,s
  y <- mul
  fit1 <- gev.fit(x,y,mul=c(1:2))
  fit1[["df"]] <- 5
  y <- sigl
  fit2 <- gev.fit(x,y,sigl=c(1:2))
  fit2[["df"]] <- 5
  y <- shl
  fit3 <- gev.fit(x,y,shl=c(1:2))
  fit3[["df"]] <- 5
  # double, s, single
  y <- cbind(single,mul)
  fit4 <- gev.fit(x,y,mul=c(2:3), sigl = 1)
  fit4[["df"]] <- 6
  fit5 <- gev.fit(x,y,mul=c(2:3), shl = 1)
  fit5[["df"]] <- 6
  y <- cbind(single,sigl)
  fit6 <- gev.fit(x,y,mul=1, sigl = c(2:3))
  fit6[["df"]] <- 6
  fit7 <- gev.fit(x,y,sigl = c(2:3), shl = 1)
  fit7[["df"]] <- 6
  y <- cbind(single,shl)
  fit8 <- gev.fit(x,y,mul=1, shl = c(2:3))
  fit8[["df"]] <- 6
  fit9 <- gev.fit(x,y,sigl = 1, shl = c(2:3))
  fit9[["df"]] <- 6
  # double, single, single
  y <- cbind(single,mul)
  fit10 <- gev.fit(x,y,mul=c(2:3), sigl = 1, shl = 1)
  fit10[["df"]] <- 7
  y <- cbind(single,sigl)
  fit11 <- gev.fit(x,y,mul=1, sigl = c(2:3), shl = 1)
  fit11[["df"]] <- 7
  y <- cbind(single,shl)
  fit12 <- gev.fit(x,y,mul=1, sigl = 1,shl = c(2:3))
  fit12[["df"]] <- 7
  # double, double, s
  y <- cbind(mul, sigl)
  fit13 <- gev.fit(x,y,mul=c(1:2), sigl = c(3:4))
  fit13[["df"]] <- 7
  y <- cbind(mul, shl)
  fit14 <- gev.fit(x,y,mul=c(1:2), shl = c(3:4))
  fit14[["df"]] <- 7
  y <- cbind(sigl, shl)
  fit15 <- gev.fit(x,y,sigl=c(1:2), shl = c(3:4))
  fit15[["df"]] <- 7
  
  # double, double, single
  y <- cbind(single, mul, sigl)
  fit16 <- gev.fit(x,y,mul=c(2:3), sigl = c(4:5), shl = 1)
  fit16[["df"]] <- 8
  y <- cbind(single, mul, shl)
  fit17 <- gev.fit(x,y,mul=c(2:3), sigl = 1, shl = c(4:5))
  fit17[["df"]] <- 8
  y <- cbind(single, sigl, shl)
  fit18 <- gev.fit(x,y,mul=1, sigl = c(2:3), shl = c(4:5))
  fit18[["df"]] <- 8
  # double, double, double
  y <- cbind(mul, sigl, shl)
  fit19 <- gev.fit(x,y,mul=c(1:2), sigl = c(3:4), shl = c(5:6))
  fit19[["df"]] <- 9
  AIC <- numeric(19)
  for(i in 1:19){
    AIC[i] <- 2*(get(paste0("fit",i))$nllh+get(paste0("fit",i))$df)
  }
  fit <- get(paste0("fit", which.min(AIC)))
  fit[["which is double"]] <- model[which.min(AIC)]
  return(fit)
}
