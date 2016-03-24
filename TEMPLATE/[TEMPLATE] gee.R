library(doBy)
library(geepack)
warp.gee <- geeglm(breaks ~ tension, id=wool, family=gaussian, 
	data=warpbreaks)
summary(warp.gee)
LSmeans(warp.gee,effect="tension")