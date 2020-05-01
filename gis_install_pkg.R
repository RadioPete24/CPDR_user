pkg_list <- c("sp"
              , "broom"
              , "rgdal"
              , "rgeos"
              , "raster"
              , "rworldmap"
              , "dismo"
              , "data.table"
              , "htmlwidgets"
              , "maptools"
              , "KernSmooth"
              , "rangeMapper"
              , "fs"
              , "sf"
              , "leaflet"
              , "lubridate"
              #, "tigris"
              #, "acs"
              , "shiny"
              , "shinythemes"
              #, "SDMTools"
              , "xlsx"
              , "XLConnect"
              , "foreign"
              , "openxlsx"
              , "leaflet.extras"
              , "googleVis"
              , "scales"
              , "ggmap"
              , "RColorBrewer"
              , "gridExtra"
              , "maps"
              , "tmap"
              , "tmaptools"
              #, "zipcode"
)
pkg_list_new <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
if(length(pkg_list_new)>0) {
  install.packages(pkg_list_new)
}

lapply(pkg_list, require, character.only = TRUE)

# # load the needed libraries
# library(ggplot2)
# library(ggmap)
# 
# # get the map
# m <- get_map(location=c(lon=0,lat=0),zoom=5)
# 
# # create the breaks- and label vectors
# ewbrks <- seq(-10,10,5)
# nsbrks <- seq(-10,10,5)
# ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(x, "°E"), ifelse(x > 0, paste(x, "°W"),x))))
# nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "°S"), ifelse(x > 0, paste(x, "°N"),x))))
# 
# # create the map
# ggmap(m) +
#   geom_blank() +
#   scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
#   scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
#   theme(axis.text = element_text(size=12))