pre_ydat2 <- function(y){
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
        if(y[40,i]==40){
          ydat[41:100,i] <- 41:100
        }else{
          ydat[41:100,i] <-((sqrt(y[40,i])+1):(sqrt(y[40,i])+60))^2
        }
      }
    }
  }
  return(ydat) 
}