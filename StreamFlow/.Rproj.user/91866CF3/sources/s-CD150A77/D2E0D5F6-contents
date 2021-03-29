CI <- function(x, mle, cov){
  est <- matrix(NA, ncol=3,nrow=nrow(x))
  for(i in 1:nrow(x)){
    b <- x[i,]
    est[i,1] <- t(b) %*% mle
    est[i,2] <- t(b) %*% mle+qnorm(0.975)*sqrt(t(b)%*%cov%*%b)
    est[i,3] <- t(b) %*% mle-qnorm(0.975)*sqrt(t(b)%*%cov%*%b)
  }
  est <- as.data.frame(est)
  est <- cbind(1:40, est)
  names(est) <- c("t","est","U","L")
  return(est)
}

