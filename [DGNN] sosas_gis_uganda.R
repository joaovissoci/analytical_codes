#library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(gridExtra)
library(ggmap)
library(maptools) 

#get.centroids = function(x){   # extract centroids from polygon with given ID
#  poly = wmap@polygons[[x]]
#  ID   = poly@ID
#  centroid = as.numeric(poly@labpt)
#  return(c(id=ID, c.long=centroid[1], c.lat=centroid[2]))
#}

wmap   <- readShapePoly("/home/joao/Desktop/uganda_districts_2014/uganda_districts_2014.shp")
wmap.df <- fortify(wmap)                # data frame for world map

ggmap   <- ggplot(wmap.df, aes(x=long, y=lat, group=group))
ggmap   <- ggmap + geom_polygon()
ggmap   <- ggmap + geom_path(colour="white", size=.2)
