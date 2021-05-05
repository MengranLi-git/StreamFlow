return_level <- function(fit,p){
  mu <- fit$est_mu[,2:4]
  sig <- exp(fit$est_sig[,2:4])
  sh <- fit$est_sh[,2:4]
  r_l <- mu-sig/sh*(1-(-log(1-p))^(-sh))
  r_l$Year <-  1980:2019
  names(r_l) <- c("est","U","L","Year")
  return(r_l)
}