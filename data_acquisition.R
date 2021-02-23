library(EGRET)
library(dataRetrieval)

gauge <- read.table("gauge_information.txt", header = FALSE)
names(gauge) <- gauge[1, ]
gauge <- gauge[-1, ]

Region_name <- read.table("Region_name.txt", header = FALSE, sep=",")
names(Region_name) <- Region_name[1, ]
Region_name <- Region_name[-1, ]

# This parameter refers to streamflow dataset
parameterCd <- "00060"

{
  # EGRET
  Streamflow <- lapply(
    gauge[, 2], readNWISDaily, parameterCd,
    "1980-01-01", "2019-12-31"
  )
  
  # site information from filelist2
  siteInfo <- lapply(filelist2, attr, "siteInfo")
  
  names(siteInfo) <- paste0("S", gauge[, 2])
  invisible(lapply(names(siteInfo), function(x) assign(x, siteInfo[[x]], envir = .GlobalEnv)))
  Siteinfo <- Reduce(function(x, y) merge(x, y, by = names(S14301000), all.x = T, all.y = T), siteInfo)
  
  # read site information directly by readNWISsite
  #  site <- readNWISsite(gauge[, 2])
  #  site2 <- readNWISInfo(gauge[, 2], "00060", interactive = FALSE)
}

for (i in 1:length(Streamflow)) {
  Streamflow[[i]]$Region <- gauge[i, 1]
  Streamflow[[i]]$Site <- gauge[i, 2]
}

Stream <- rbindlist(Streamflow)[
  ,DecYear := floor(DecYear)] %>%
  filter(DecYear != 2020)

rm(Streamflow)

save(gauge, Region_name, Siteinfo, Stream, file = "Stream.Rdata")
