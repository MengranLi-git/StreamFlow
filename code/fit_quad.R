fit_quad <- function(x, para=c("quad", "single", "s")) {
  co <- data.frame(mu = NA, sig = NA, sh = NA)
  n <- 1
  for (i in 1:3) {
    for (j in 1:3) {
      for (h in 1:3) {
        co[n, 1] <- para[i]
        co[n, 2] <- para[j]
        co[n, 3] <- para[h]
        n <- n + 1
      }
    }
  }
  single <- 1:length(x)
  double_mu <- cbind(1:length(x),(1:length(x))^2)
  double_sig <- cbind(1:length(x),(1:length(x))^2)
  double_sh <- cbind(1:length(x),(1:length(x))^2)
  
  rank <- which(apply(co,1,function(x) "quad" %in% x))
  for (i in rank) {
    com <- co[i, ]
    
    ll_mu <- ll(com$mu, para, double_mu)
    ll_sig <- ll(com$sig, para, double_sig)
    ll_sh <- ll(com$sh, para, double_sh)
    
    y <- cbind(1, ll_mu[[2]], ll_sig[[2]], ll_sh[[2]])
    
    if (is_empty(ll_mu[[1]])) {
      mul <- NULL
      loc_mu <- 1
    } else {
      mul <- ll_mu[[1]] + 1
      loc_mu <- c(1, mul)
    }
    
    if (is_empty(ll_sig[[1]])) {
      sigl <- NULL
      loc_sig <- max(loc_mu) + 1
    } else {
      if (is_empty(mul)) {
        sigl <- 1 + ll_sig[[1]]
        loc_sig <- c(max(loc_mu), sigl) + 1
      } else {
        sigl <- max(mul) + ll_sig[[1]]
        loc_sig <- c(max(loc_mu), sigl) + 1
      }
    }
    
    if (is_empty(ll_sh[[1]])) {
      shl <- NULL
      loc_sh <- max(loc_sig) + 1
    } else {
      if (is_empty(sigl)) {
        if (is_empty(mul)) {
          shl <- 1 + ll_sh[[1]]
          loc_sh <- c(max(loc_sig), shl+1) + 1
        } else {
          shl <- max(mul) + ll_sh[[1]]
          loc_sh <- c(max(loc_sig), shl+1) + 1
        }
      } else {
        shl <- max(sigl) + ll_sh[[1]]
        loc_sh <- c(max(loc_sig), shl+1) + 1
      }
    }
    
    in2 <- log(sqrt(6 * var(x,na.rm=TRUE)/pi))
    siginit <- c(in2, rep(0, length(sigl)))
    fit <- try(gev.fit(x, y, mul = mul, sigl = sigl, shl = shl,siglink = exp, siginit = siginit, show = FALSE),silent=TRUE)
    if('try-error' %in% class(fit)){
      fit <- NULL
      name <- paste0("fit", i)
      assign(name, fit)
      next
    }else{
      fit <- gev.fit(x, y, mul = mul, sigl = sigl, shl = shl,siglink = exp, siginit = siginit, show = FALSE)
      fit[["AIC"]] <- 2 * (length(fit$mle) + fit$nllh)
      fit[["formulation"]] <- com
      fit[["est_mu"]] <- CI(as.matrix(cbind(rep(1, 40), ll_mu[[2]])), fit$mle[loc_mu], fit$cov[loc_mu, loc_mu])
      
      fit[["est_sig"]] <- CI(as.matrix(cbind(rep(1, 40), ll_sig[[2]])), fit$mle[loc_sig], fit$cov[loc_sig, loc_sig])
      fit[["est_sh"]] <- CI(as.matrix(cbind(rep(1, 40), ll_sh[[2]])), fit$mle[loc_sh], fit$cov[loc_sh, loc_sh])
    }
    name <- paste0("fit", i)
    assign(name, fit)
  }
  AIC <- rep(100000,27)
  for (i in rank) {
    if(!is_empty(get(paste0("fit", i)))){
      AIC[i] <- get(paste0("fit", i))$AIC
    }
  }
  fit <- get(paste0("fit", which.min(AIC)))
  return(fit)
}
