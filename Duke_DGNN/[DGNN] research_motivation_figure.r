data1<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGNN/motivation_research/data1.csv")
data2<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGNN/motivation_research/data2.csv")

#data1[,1]<-as.character(data1[,1])
#data1[,2]<-as.character(data1[,2])

edgelist = as.matrix(data1[,1:2]) #lista de entrada e saida
new_ord = c(1,3,2,4,6,7,5,8,10,11,9,17,19,18,20,22,23,24,25,21,26,28,27,29,31,30,12,14,15,16,13) #ordem dos itens na figura
#vlabels = data2$labels #label dos nodos
vborders = c(rep("darkred",3),rep("darkred",4),rep("darkred",4),rep("steelblue4",5),rep("goldenrod4",3),rep("goldenrod4",6),rep("goldenrod4",3),rep("steelblue4",3))
vfill = c("red","darkred","red","red","darkred","red","red","red","darkred","red","red","steelblue","steelblue4","steelblue","steelblue","steelblue","gold","goldenrod4","gold","gold","goldenrod4","gold","gold","gold","gold","gold","goldenrod4","gold","steelblue","steelblue4","steelblue")
degrees = c(1,10,1,1,10,1,1,1,10,1,1,1,10,1,1,1,1,10,1,1,10,1,1,1,1,1,10,1,1,10,1) #tamanho dos nodos
values = data1$value
#arclist=c(1:23)#horizontal
arclist=c(24:31)#horizontal
lablecolor=c(rep("darkred",3),rep("darkred",4),rep("darkred",4),rep("steelblue4",5),rep("goldenrod4",3),rep("goldenrod4",6),rep("goldenrod4",3),rep("steelblue4",3))

linenodes=c(-24,-25,-12,-21.5,-35.5,-25,-12,-29,-28,-28.5,-12,-33,-30.8,-12,-27.4,-25.3,-27,-26.8,-32.6,-12,-31.5,-23,-12,-36.5,-24.8,-12,-32.8,-27.7,-31,-33.5,-12)
#linenodes=c(-18,-25,-25.7,-18,-23.4,-32.7,-25.7,-18,-28.2,-27.7,-28,-18,-31,-29.5,-18,-27.2,-25.9,-27.1,-27,-30.9,-18,-30,-24.8,-18,-33.8,-26.2,-18,-31,-28,-30.3,-32)

# plot arc diagram
#dev.new(width=15, height=14)

tiff("/Users/joaovissoci/Desktop/fig_motivation_research.tiff", width = 700, height = 1200,compression = 'lzw')
arcplot(edgelist, ordering=new_ord, cex.labels = 1.2,
        show.nodes = TRUE, cex.nodes = log(degrees) + 3.0, pch.nodes = 21, lwd.nodes = 2, line = linenodes,col.nodes = vborders,
        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values,bg.nodes = vfill,above=arclist,col.labels=lablecolor,horizontal=FALSE) #col.nodes = vborders,, labels = vlabels,
legend("bottomright",c("Writing Skills","Regulated motivation","Autonomous motivation"),pch=rep('O',2),col=c("darkred","goldenrod4","steelblue4"),box.lty=0,cex=1.2)
legend("bottomleft",c("Themes","Sub-themes"),lty=rep(1,2),lwd=c(15.5,5),col=c("grey","grey"),box.lty=0,cex=1.2)
#locator()
dev.off()

