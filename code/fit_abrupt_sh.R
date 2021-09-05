fit_abrupt_sh <- function(x, mu, sig, fix.sh, sh.se, para=c("break", "single", "s")) {
  co <- data.frame(mu = NA, sig = NA)
  n <- 1
  for (i in 1:3) {
    for (j in 1:3) {
        co[n, 1] <- para[i]
        co[n, 2] <- para[j]
        n <- n + 1
    }
  }
  single <- 1:length(x)
  double_mu <- cut_breakpoint(1:length(x), mu, TRUE)
  double_sig <- cut_breakpoint(1:length(x), sig, TRUE)
  
  rank <- which(apply(co,1,function(x) "break" %in% x))
  
  for (i in rank) {
    com <- co[i, ]
    
    ll_mu <- ll(com$mu, para, double_mu,TRUE)
    ll_sig <- ll(com$sig, para, double_sig,TRUE)
    
    y <- cbind(1, ll_mu[[2]], ll_sig[[2]])
    
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
    
    in2 <- log(sqrt(6 * var(x,na.rm = TRUE)/pi))
    siginit <- c(in2, rep(0, length(sigl)))
    fit <- try(gev_fix(x, y, mul = mul, sigl = sigl, siglink = exp, siginit = siginit, fix.sh=fix.sh, show = FALSE),silent=TRUE)
    if('try-error' %in% class(fit)){
      fit <- NULL
      name <- paste0("fit", i)
      assign(name, fit)
      next
    }else{
      fit <- gev_fix(x, y, mul = mul, sigl = sigl, siglink = exp, siginit = siginit, fix.sh=fix.sh, show = FALSE)
      fit[["AIC"]] <- 2 * (length(fit$mle) + fit$nllh)
      fit[["formulation"]] <- com
      fit[["est_mu"]] <- CI(as.matrix(cbind(rep(1, 40), ll_mu[[2]])), fit$mle[loc_mu], fit$cov[loc_mu, loc_mu])
      fit[["est_sig"]] <- CI(as.matrix(cbind(rep(1, 40), ll_sig[[2]])), fit$mle[loc_sig], fit$cov[loc_sig, loc_sig])
      est_sh <- matrix(NA, ncol=3,nrow=length(x))
      est_sh[,1] <- fix.sh
      est_sh[,2] <- fix.sh + sh.se
      est_sh[,3] <- fix.sh - sh.se
      est_sh <- as.data.frame(est_sh)
      est_sh <- cbind(1:40, est_sh)
      names(est_sh) <- c("t","est","U","L")
      fit[["est_sh"]] <- est_sh
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
