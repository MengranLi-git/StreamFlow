graph_data <- function(fit, name=c("best_fit","s_fit","single_fit","double_fit","quad_fit")){

  est_mu <- NULL
  est_sig <- NULL
  est_sh <- NULL
  for(i in 1:length(fit)){
    est_mu <- rbind(est_mu,fit[[i]]$est_mu)
    est_sig <- rbind(est_sig,fit[[i]]$est_sig)
    est_sh <- rbind(est_sh,fit[[i]]$est_sh)
  }
  
  est_mu$Group <- rep(name,each=40)
  est_sig$Group <- rep(name,each=40)
  est_sh$Group <- rep(name,each=40)
  est <- list()
  est[["est_mu"]] <- est_mu
  est[["est_sig"]] <- est_sig
  est[["est_sh"]] <- est_sh
  return(est)
}

