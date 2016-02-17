
https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf


library(ggmap)

set.seed(500)
df <- round(data.frame(
x = jitter(rep(-95.36, 50), amount = .3),
y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap('houston', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')
qmap(baylor, zoom = 14, source = "stamen", maptype = "watercolor")
qmap(baylor, zoom = 14, source = "stamen", maptype = "toner")

violent_crimes <- subset(crime,
 	offense != "auto theft" & offense != "theft" & offense != "burglary")

violent_crimes$offense <- factor(violent_crimes$offense,
 	levels = c("robbery", "aggravated assault", "rape", "murder"))

violent_crimes <- subset(violent_crimes,
 -95.39681 <= lon & lon <= -95.34188 &
 29.73631 <= lat & lat <= 29.78400)

library(ggmap)

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)

ggmap(map, extent = "device")

downtown <- subset(crime,
  -95.39681 <= lon & lon <= -95.34188 &
   29.73631 <= lat & lat <=  29.78400
)

qmplot(lon, lat, data = downtown, maptype = "toner-background", 
	color = I("red"))

qmplot(lon, lat, data = downtown, 
	maptype = "toner-lite", geom = "density2d", color = I("red"))

robberies <- subset(downtown, offense == "robbery")

qmplot(lon, lat, data = downtown, geom = "blank", 
	zoom = 15, maptype = "toner-background", darken = .7) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", 
  	alpha = .3, color = NA) +
  scale_fill_gradient2("Robbery\nPropensity", 
  	low = "white", mid = "yellow", high = "red", 
  	midpoint = 1500)

qmplot(lon, lat, data = downtown, maptype = "toner-background", 
	color = offense) + 
  facet_wrap(~ offense)


#library(raster)
#library(rgeos)
#box <- as(extent(as.numeric(attr(map, 'bb'))[c(2,4,1,3)] + 
#    c(.001,-.001,.001,-.001)), "SpatialPolygons")
#proj4string(box) <- CRS(summary(tract)[[4]])
#tract <- readOGR(dsn = ".", layer = "gz_2010_13_140_00_500k")
#tractSub <- gIntersection(tract, box, byid = TRUE, 
#    id = as.character(tract$GEO_ID))
#tractSub <- fortify(tractSub, region = "GEO_ID")
#plotData <- left_join(tractSub, data, by = "id")

#library(rgdal)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(gridExtra)
library(ggmap)
library(maptools) 
library(grid)

wmap   <- readShapePoly("/Users/joaovissoci/Gits/analytical_codes/shapefiles/ugada_2014/uganda_districts_2014.shp")
wmap_df <- fortify(wmap)                # data frame for world map
wmap_poly <- merge(wmap_df,wmap,by="D_06_ID") # merge data to fill polygons

data_points<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGNN/SOSAS/SOSAS_gis/abd_conditions_sosas.csv")

SFNeighbourhoods  = merge(wmap_df, wmap@data, 
	by.x = 'id', by.y = 'ZIP_CODE')


#maps can be: "terrain", "terrain-background", "satellite", 
#"roadmap", and "hybrid" (google maps), "terrain", 
#"watercolor", and "toner" (stamen maps), 
#or a positive integer for cloudmade maps (see ?get_cloudmademap)
map <- get_map("Uganda", zoom = 7, maptype = "roadmap")
p<- ggmap(map) + geom_polygon(data = wmap_df, 
	aes(x = long, y = lat, group = group), 
	colour = "black", fill="black",alpha = 0,
	size=.2) + geom_path()
p 
p <- p + geom_point(data=data_points,aes(
	x=ead_long,y=ead_lat,size = X..Untreated.Abd.Cond))
p
p <- p + scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), 
        labels = percent)

p <- p + labs(fill = "")

p <- p + guides(fill = guide_legend(reverse = TRUE, override.aes = 
        list(alpha = 1)))


ggmap   <- ggplot(wmap_df, aes(x=long, y=lat, group=group))
ggmap   <- ggmap + geom_polygon(fill="white")
ggmap   <- ggmap + geom_path(colour="black", size=.2)
ggmap   <- ggmap + geom_point(data=data_points,aes(
	x=ead_long,y=ead_lat,size = X..Untreated.Abd.Cond))
ggmap



ggsave(p, file = "map9.png", width = 5, height = 4, type = "cairo-png")\




