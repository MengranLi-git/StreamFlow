cut_breakpoint <- function(x, breakpoint=0, abrupt = FALSE){
  if(length(breakpoint) == 0){
    return(NULL)
  }else{
    if(length(breakpoint) == 1 & breakpoint[1]==0){
      return(as.matrix(x)) 
  }
}
      breakpoint <- c(-Inf,breakpoint,+Inf)
      nr <- length(x)
      nc <- length(breakpoint)
      D <- matrix(NA, nrow=nr, ncol=nc-1)
      
      for (i in 2:nc){
        D[,i-1] <- ifelse(x>breakpoint[i-1]&x<=breakpoint[i],1,0)
      }
      data <- matrix(NA, nrow=nr, ncol=nc-1)
      data[,1] <- (x-breakpoint[2])*D[,1]
      for(i in 2:(nc-1)){
        data[,i] <- (x-breakpoint[i])*D[,i]
      }
  if(abrupt == FALSE){
    return(data)
  }else{
    return(cbind(D,data))
  }
}