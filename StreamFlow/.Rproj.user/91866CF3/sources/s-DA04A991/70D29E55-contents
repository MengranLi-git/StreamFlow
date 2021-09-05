GetStation <- function(x){
  url <- paste0("F:/StreamFlow/Data_Acquistion/",x,".txt")
  
  gauge <- read.table(url, header = FALSE)
  gauge[,2] <- as.character(gauge[,2])
  gauge[,2] <- paste0("0",gauge[, 2])
  
  parameterCd <- "00060"
  Streamflow <- lapply(
    gauge[, 2], readNWISDaily, parameterCd,
    "1980-01-01", "2019-12-31"
  )
  index <- which(sapply(Streamflow, nrow)==14610)
  gauge<- gauge[index,]
  Streamflow <- Streamflow[index]
  Streamflow <- rbindlist(Streamflow)[
    ,DecYear := floor(DecYear)] %>%
    filter(DecYear != 2020)
  result <- list(readNWISsite(gauge[, 2]), Streamflow)
  return(result)
}


mississippi <- GetStation("mississippi")
des <- GetStation("des")
illinois <- GetStation("illinois")
iowa <- GetStation("iowa")
minnesota <- GetStation("minnesota")
wapsipinicon <- GetStation("wapsipinicon")



new <- rbind(mississippi[[1]], des[[1]])
new <- rbind(new, illinois[[1]])
new <- rbind(new, iowa[[1]])
new <- rbind(new, minnesota[[1]])
new <- rbind(new, wapsipinicon[[1]])

data <- rbind(mississippi[[2]], des[[2]])
data <- rbind(data, illinois[[2]])
data <- rbind(data, iowa[[2]])
data <- rbind(data, minnesota[[2]])
data <- rbind(data, wapsipinicon[[2]])


save(new, data, file="F:/StreamFlow/joint_fitting/joint.Rdata")






