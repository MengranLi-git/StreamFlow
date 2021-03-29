fit_single <- function(x,y){
  para <- c(NULL,1)
  mul=
  fit1 <- gev.fit(x, y, mul=1)
  fit1[["df"]] <- 4
  fit2 <- gev.fit(x, y, sigl=1)
  fit2[["df"]] <- 4
  fit3 <- gev.fit(x, y, shl=1)
  fit3[["df"]] <- 4
  fit4 <- gev.fit(x, y, mul=1, sigl=1)
  fit4[["df"]] <- 5
  fit5 <- gev.fit(x, y, mul=1, shl=1)
  fit5[["df"]] <- 5
  fit6 <- gev.fit(x, y, sigl=1, shl=1)
  fit6[["df"]] <- 5
  fit7 <- gev.fit(x, y, mul=1, sigl=1, shl=1)
  fit7[["df"]] <- 6
  model=c("mul","sigl","shl","mul,sigl","mul,shl","sigl,shl","mul,sigl,shl")
  AIC <- numeric(7)
  for(i in 1:7){
    AIC[i] <- 2*(get(paste0("fit",i))$nllh+get(paste0("fit",i))$df)
  }
  fit <- get(paste0("fit", which.min(AIC)))
  fit[["which is single"]] <- model[which.min(AIC)]
  return(fit)
}
