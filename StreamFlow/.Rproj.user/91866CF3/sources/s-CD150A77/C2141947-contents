Moving_window <- function(x, win=15, url, save=TRUE, name){
  n_row = nrow(x)
  n_sep = round(win/2)
  #  fit <- gev.fit(x$V1)
  #  shape <- fit$mle[3]
  parameter <- as.data.frame(matrix(NA, ncol=4, nrow=n_row))
  names(parameter) <- c("t","location","scale","shape")
  for (i in 1:(n_row-2*n_sep)){
    fit <- gev.fit(x$V1[i:(i+win)])
    parameter[i+n_sep,1] <- i+n_sep
    parameter[i+n_sep,2:4] <- fit$mle
  }
  if(save == TRUE){
    ggplot(parameter, aes(x=t, y=location))+
      geom_point()+
      xlab("t")+
      ylab("location")+
      ggsave(paste0(url,"/","location",name,".png"), plot=last_plot(),height=3,width=4)
    ggplot(parameter, aes(x=t, y=scale))+
      geom_point()+
      xlab("t")+
      ylab("scale")
    ggsave(paste0(url,"/","scale",name,".png"), plot=last_plot(),height=3,width=4)
    ggplot(parameter, aes(x=t, y=shape))+
      geom_point()+
      xlab("t")+
      ylab("shape")
    ggsave(paste0(url,"/","shape",name,".png"), plot=last_plot(),height=3,width=4)
    
  }
  return(parameter)
}