
####################################
#Save figures to .tff format
####################################

#makes figures very big in size
tiff("/Users/jnv4/Desktop/figure1B.tiff",
 width = 2100, height = 3500,compression = 'lzw', res=300)
#Add plot
dev.off()

####################################
#Save figures to .eps format
####################################
setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/jnv4/Desktop/figure1B.eps",
	width = 8, height = 12)
#Add plot
dev.off()

####################################
#Save GGPLOT figure in EPS of PDF
####################################

#generate plot within an object (hereby named - plot)
plot<-ggplot(mtcars, aes(mpg, wt)) + geom_point()

#save figure
ggsave("figure2.eps", #change .eps to .pdf for different format
		plot, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
		path="/Users/jnv4/Desktop", #path to save the plot
		width = 7.5, 
		height = 4.3, 
		device=cairo_ps) #cairo_ps is a device to save eps with transparecy