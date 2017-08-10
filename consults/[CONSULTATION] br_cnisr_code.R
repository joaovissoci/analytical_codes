

years<-c(2016,2013,2017,2014,2010,2003,2015,2012,2011,2004,2005,2006,2007,2008,2009)
freq<-c(10,3,1,5,1,1,5,4,1,0,0,0,0,0,0)

x<-data.frame(years,freq)

install.packages("plotly")

library(plotly)
#plotting time series by month
ggplot(x,aes(years,freq)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  # scale_x_date(format = "%Y")
  # theme(plot.title = element_text(lineheight=.7, face="bold"))

referenceLines <- x  # \/ Rename


zp <- ggplot(x,
              aes(x = years, y = freq))
zp <- zp + geom_line(data = referenceLines,  # Plotting the "underlayer"
                       aes(x = years, y = freq),
                       colour = "GRAY", alpha = 1/2, size = 1/2)
zp <- zp + geom_line(size = 1)  # Drawing the "overlayer"
# zp <- zp + facet_wrap(~ Month)
zp <- zp + theme_bw() + xlab("Anos") + ylab("# de estudos")

# ggplotly()

# myData <- data.frame(Year = c(floor(time(AirPassengers) + .01)),
#                      Month = c(cycle(AirPassengers)),
#                      Value = c(AirPassengers))

zp + scale_x_continuous(breaks=seq(2003, 2017, 1)) +
	 scale_y_continuous(breaks=seq(0, 10, 1)) 

library("ggmap")
library(maptools)
library(maps)

visited <- c("China", "South Africa", "Turkey", "United Stated", 
			 "United Kingdom", "Tanzania", "Australia","Kenia",
			 "Canada","Lebanon","Ghana","Mexico","Nigeria",
			 "Germany","Peru","New Guinea","Ethiopia")

size<-c(4,7,1,7,1,1,1,10,1,1,1,1,1,7,4,1,1)
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat

#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="indianred1", size=size) 
mp <- mp + xlab("Longitude") + ylab("Latitute")
mp <- mp + scale_color_gradient2(low="green", mid="red",high="blue", midpoint=18,
                        breaks=c(10,20,30), labels=c("ten","twenty","thirty"))


