fit_abrupt <- function(x,mu,sig,sh){
  single <- cut_breakpoint(1:40, breakpoint=0)
  mul <- cut_breakpoint(1:40, mu)
  sigl <- cut_breakpoint(1:40, sig)
  shl <- cut_breakpoint(1:40, sh)
  mul_a <- cut_breakpoint(1:40, mu, TRUE) 
  sigl_a <- cut_breakpoint(1:40, sig, TRUE)
  shl_a <- cut_breakpoint(1:40, sh, TRUE)
  # double_a, s,s
  y <- mul_a
  fit1 <- gev.fit(x,y,mul=c(1:4))
  y <- sigl_a
  fit2 <- gev.fit(x,y,sigl=c(1:4))
  y <- shl_a
  fit3 <- gev.fit(x,y,shl=c(1:4))
  
  # double_a, s, single
  y <- cbind(single,mul_a)
  fit4 <- gev.fit(x,y,mul=c(2:5), sigl = 1)
  fit5 <- gev.fit(x,y,mul=c(2:5), shl = 1)
  y <- cbind(single,sigl_a)
  fit6 <- gev.fit(x,y,mul=1, sigl = c(2:5))
  fit7 <- gev.fit(x,y,sigl = c(2:5), shl = 1)
  y <- cbind(single,shl_a)
  fit8 <- gev.fit(x,y,mul=1, shl = c(2:5))
  fit9 <- gev.fit(x,y,sigl = 1, shl = c(2:5))
  
  # double_a, single, single
  y <- cbind(single,mul_a)
  fit10 <- gev.fit(x,y,mul=c(2:5), sigl = 1, shl = 1)
  fit11 <- gev.fit(x,y,mul=c(2:5), sigl = 1, shl = 1)
  y <- cbind(single,sigl_a)
  fit12 <- gev.fit(x,y,mul=1, sigl = c(2:5), shl = 1)
  fit13 <- gev.fit(x,y,mul=1, sigl = c(2:5), shl = 1)
  y <- cbind(single,shl_a)
  fit14 <- gev.fit(x,y,mul=1, sigl = 1,shl = c(2:5))
  fit15 <- gev.fit(x,y,mul=1, sigl = 1, shl = c(2:5))
  
  # double_a, double_a, s
  y <- cbind(mul_a, sigl_a)
  fit16 <- gev.fit(x,y,mul=c(1:4), sigl = c(5:8))
  y <- cbind(mul_a, shl_a)
  fit17 <- gev.fit(x,y,mul=c(1:4), shl = c(5:8))
  y <- cbind(sigl_a, shl_a)
  fit18 <- gev.fit(x,y,sigl=c(1:4), shl = c(5:8))
  
  # double_a, double_a, single
  y <- cbind(single, mul_a, sigl_a)
  fit19 <- gev.fit(x,y,mul=c(2:5), sigl = c(6:9), shl = 1)
  y <- cbind(single, mul_a, shl_a)
  fit20 <- gev.fit(x,y,mul=c(2:5), sigl = 1, shl = c(6:9))
  y <- cbind(single, sigl_a, shl_a)
  fit21 <- gev.fit(x,y,mul=1, sigl = c(2:5), shl = c(6:9))
  
  # double_a, double_a, double_a
  y <- cbind(mul_a, sigl_a, shl_a)
  fit21 <- gev.fit(x,y,mul=c(1:4), sigl = c(5:8), shl = c(9:12))
  
  log <- numeric(21)
  for(i in 1:21){
    log[i] <- get(paste0("fit",i))$nllh
  }
  return(log)
}
