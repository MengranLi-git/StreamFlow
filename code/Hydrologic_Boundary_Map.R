library(ggplot2)
library(rgeos)
vessel <- sf::st_read(dsn = "F:/StreamFlow/wbdhu2_a_us_september2020.gdb")
vessel <- subset(vessel, !huc2 %in% c("19","20","21","22"))
ggplot() + 
  geom_sf(data = vessel) + 
  ggtitle("Hydrologic Boundary Plot") + 
  coord_sf()

