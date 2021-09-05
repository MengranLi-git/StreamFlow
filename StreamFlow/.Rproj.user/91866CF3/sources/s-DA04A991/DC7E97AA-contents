library(ismev)
library(evd)
library(tidyverse)


source("F:/Extre/Codes/Functions.R")
load("F:/StreamFlow/joint_fitting/joint.Rdata")

# June July Aug

data <- data %>% filter(Month %in% 6:8)
data$station <- rep(new$site_no,each = 3680)

#### GEV marginal ####
SummerMaxima <- data %>% group_by(station, DecYear) %>% summarise(max(Q)) %>% spread(station, `max(Q)`)

method <- "BFGS" 
control <- list(maxit = 1000, reltol=10^(-30), abstol=0.0001, trace=2)

Maxima <- SummerMaxima

StsChos <- c(1:27)
NoSt <- length(StsChos)

ResGev <- list()
GevPars <- matrix(0, NoSt, 3)
FreeGevNLLTemp <- numeric()
for (i in 1:NoSt){
  ResGev[[i]] <- gev.fit(as.matrix(Maxima[,i+1]))
  GevPars[i,] <- ResGev[[i]]$mle
  FreeGevNLLTemp[i] <- ResGev[[i]]$nll
}


#### Extraction of univariate events  ####

U <- 0.9
Lag <- 5
StNames <- as.character(1:27)
mfrow <- c(1,1)
YearsWithEvents <- list()
Threshold <- numeric()
AllEvents <- list()
NoOfYears <- numeric()
for (i in 1:NoSt) {
  index <- ((i-1)*3680+1):(i*3680)
  TSs <- data[index,]
  TSs <- TSs %>% select(Date, Q)
  names(TSs) <- c("Date", "Val")
  X <- TSs$Val
  Threshold[i] <- quantile(X,U)
  Years <- unique(as.numeric(substr(TSs$Date,1,4)))
  Events <- numeric()
  YearEvents <- numeric()
  for (j in 1:length(Years)) {
    X1 <- ObtainMultVarEvents(TSs = TSs,U=U,Lag=Lag,Year=Years[j],StNames=StNames[i],Plotting=0,mfrow)
    X2 <- as.numeric(X1$AllEvMat)
    Events  <- c(Events,X2)
    YearEvents <- c(YearEvents,rep(Years[j],length(X2)))
  }
  AllEvents[[i]] <- Events
  YearsWithEvents[[i]] <- YearEvents
  NoOfYears[i] <- length(Years)
}

### Fitting a GEVD to each station (by maximizing the joint Poisson process likelihood; cf. formula (21) in the paper) ###

PPRes <- list()
ParsPP <- matrix(0,NoSt,3)
PPLLFree <- numeric()
for (i in 1:NoSt) {
  Init <- GevPars[i,]
  PPRes[[i]] <- PPFit(Data=AllEvents[i],u=Threshold[i],NoYears=NoOfYears[i],
                      Init=Init,CovarMu=matrix(1,1,1),CovarSc=matrix(1,1,1),
                      CovarXi=matrix(1,1,1),LogModel=FALSE,method=method,control=control)
  ParsPP[i,] <- PPRes[[i]]$par
  PPLLFree[i] <- PPRes[[i]]$value
}

summary(ParsPP)

M1LL <- sum(PPLLFree)


#### Multivariate events ####

data <- data %>% select(Date, Q)
data$station <- rep(new$site_no,each = 3680)
data <- data %>% spread(station, Q)

StsChos <- c(1:27)
NoSt <- length(StsChos)

Years <- unique(as.numeric(substr(data$Date,1,4)))
U <- 0.1
Lag <- 4
Plotting <- 0
StNames <- as.character(StsChos)
YearsWithEvent <- numeric()
AllEvMat <- matrix(0,length(StNames),1)
AllRes <- list()
for (i in 1:length(Years)){
  Res <- ObtainMultVarEvents(TSs=data,U=U,Lag=Lag,Year=Years[i],StNames=StNames,Plotting=Plotting,mfrow=c(NoSt,1))  ## For Censored Likelihood
  Events <- Res$AllEvMat
  Locations <- Res$Location
  if (length(Events)>0) {
    YearsWithEvent <- c(YearsWithEvent,rep(Years[i],dim(Events)[2]))
    AllEvMat <- cbind(AllEvMat,Events)
  }
  AllRes[[i]] <- Res 
}

DataEvents <- t(AllEvMat[,-1])
rownames(DataEvents) <- YearsWithEvent 


#### EC for HR ####

Theta.cen <- matrix(1,ncol = NoSt,nrow = NoSt)

for(i in 1:(NoSt-1))
  for(j in (i+1):NoSt) {
    Data.biv <- DataEvents[,c(i,j)]
    Theta.cen[i,j] <- CensoredEstimationHR(Data.biv, thresholds = .90)
    Theta.cen[j,i] <- Theta.cen[i,j]
  }


#### EC for modagram ####

## location
locations <- new %>% select(dec_long_va, dec_lat_va)
locations <- as.matrix(locations)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
library(SpatialExtremes)
##Compute the madogram
theta <- madogram(DataEvents, locations)

maxstable <- fitmaxstab(DataEvents, locations, "whitmat", nugget = 0)

## 3- Plot the extremal coefficient
extcoeff(maxstable)

CatchEucDisWt <- as.matrix(dist((locations)))

FlowCon <- read.csv("F:/StreamFlow/joint_fitting/FlowCon.csv",header = FALSE)

con <- FlowCon[lower.tri(FlowCon)]
FlowCon <- t(FlowCon)
FlowCon[lower.tri(FlowCon)] <- con

m <- matrix(NA, ncol = 27, nrow = 27)
m[lower.tri(m)] <- 1:351

theta <- as.data.frame(theta)
theta$group <- con

theta <- theta %>% mutate(
  group = ifelse(group==1, "connected", "unconnected")
)

ggplot(data = theta) + 
  geom_point(aes(x = dist, y=ext.coeff, color = as.factor(group))) +
 # coord_fixed(ratio = 10) +
  ylim(c(1,2))


#### HR VS madogram  ####

m2 <- matrix(NA, 27, 27)
m2[lower.tri(m2)] <- theta[,3]

m2 <- t(m2)
m2[lower.tri(m2)] <- theta[,3]
diag(m2) <- 1

plotECF(ECemp = m2,
        ECtheo = Theta.cen,
        Dist = CatchEucDisWt,
        is.con = FlowCon,
        StsIdx  = StsChos,
        which.plots = c(FALSE,FALSE,TRUE),
        which.labels = c(FALSE, FALSE,FALSE),
        PDF = FALSE,
        filename = paste("Plots","ECF_Emp_HR.pdf",sep="/"))

