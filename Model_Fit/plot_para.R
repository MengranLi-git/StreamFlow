plot_para <- function(est_mu, est_sig, est_sh, MOV, Region, n = 1, ignore = "best_fit") {
  p1 <- est_mu[which(est_mu$Group %in% ignore),] %>%
    ggplot(aes(x = t, y = est)) +
    geom_line(aes(color = Group, size = I(1))) +
    geom_line(aes(x = t, y = U, color = Group), linetype = 2, size = I(1)) +
    geom_line(aes(x = t, y = L, color = Group), linetype = 2, size = I(1)) +
    geom_point(data = MOV, aes(x = t, y = location, color = "window", size = I(2))) +
    geom_point(data = Region, aes(x = DecYear - 1980, y = V1, size = I(2))) +
    xlab(paste0("Region", n,"  ", "t")) +
    ylab(expression(mu(t)))
  
  p2 <- est_sig[which(est_sig$Group %in% ignore),] %>%
    ggplot(aes(x = t, y = est)) +
    geom_line(aes(color = Group, size = I(1))) +
    geom_line(aes(x = t, y = U, color = Group), linetype = 2, size = I(1)) +
    geom_line(aes(x = t, y = L, color = Group), linetype = 2, size = I(1)) +
    geom_point(data = MOV, aes(x = t, y = scale, color = "window", size = I(2))) +
    xlab(paste0("Region", n,"  ", "t")) +
    ylab(expression(sigma(t)))
  
  p3 <- est_sh[which(est_sh$Group %in% ignore),]%>%
    ggplot(aes(x = t, y = est)) +
    geom_line(aes(color = Group, size = I(1))) +
    geom_line(aes(x = t, y = U, color = Group), linetype = 2, size = I(1)) +
    geom_line(aes(x = t, y = L, color = Group), linetype = 2, size = I(1)) +
    geom_point(data = MOV, aes(x = t, y = shape, color = "window", size = I(2))) +
    xlab(paste0("Region", n,"  ", "t")) +
    ylab(expression(xi(t)))
  
  p <- list(p1, p2, p3)
  #  p <- grid.arrange(p1,p2,p3,nrow=3)
  #  ggsave(paste0("Region",n,".png"),p,path="F:/Streamflow/Plot/Fit",width=6,height=6)
  return(p)
}
