pre_ydat <- function(y){
  y <- as.matrix(y)
  n <- dim(y)[2]
  ydat <- matrix(NA,ncol=n,nrow=100)

  ydat[1:40,] <- y
  for(i in 1:n){
    if(y[40,i]==1){
      ydat[41:100,i] <- 1
    }else{
    if(y[40,i]==0){
      ydat[41:100,i] <- 0
    }else{
      ydat[41:100,i] <-(y[40,i]+1):(y[40,i]+60)
    }
   }
  }
 return(ydat) 
}

