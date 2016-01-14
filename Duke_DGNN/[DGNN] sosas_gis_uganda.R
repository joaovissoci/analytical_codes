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

wmap   <- readShapePoly("/home/joao/Git/shapefiles/ugada_2014/uganda_districts_2014.shp")
wmap.df <- fortify(wmap)                # data frame for world map

ggmap   <- ggplot(wmap.df, aes(x=long, y=lat, group=group))
ggmap   <- ggmap + geom_polygon(aes(fill="red"))
ggmap   <- ggmap + geom_path(colour="white", size=.2)
ggmap   <- ggmap + geom_point(aes(x=400000, y=10221000, label="name"),gsize=10,  color="brown2")
ggmap   <- ggmap + geom_text(aes(x=400000, y=10221000, label="name"),gsize=10,  color="brown2")
ggmap

#try to plot point from the data trying to aggregate X and Y coordinates
#if that dosn't work, then merge dataset with the values with the shapefile
#change backgroud color
#change points color
#work on cosmetics
