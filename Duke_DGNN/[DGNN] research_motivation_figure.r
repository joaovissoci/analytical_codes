data1<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGNN/motivation_research/data1.csv")
data2<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGNN/motivation_research/data2.csv")

#data1[,1]<-as.character(data1[,1])
#data1[,2]<-as.character(data1[,2])

edgelist = as.matrix(data1[,1:2]) #lista de entrada e saida
new_ord = c(2,1,3,5,4,6,7,9,8,10,11,18,17,19,21,20,22,23,24,25,27,26,28,30,29,31,13,12,14,15,16) #ordem dos itens na figura
#vlabels = data2$labels #label dos nodos
vborders = c(rep("darkred",3),rep("darkred",4),rep("darkred",4),rep("steelblue4",5),rep("goldenrod4",3),rep("goldenrod4",6),rep("goldenrod4",3),rep("steelblue4",3))
vfill = c("red","darkred","red","red","darkred","red","red","red","darkred","red","red","steelblue","steelblue4","steelblue","steelblue","steelblue","gold","goldenrod4","gold","gold","goldenrod4","gold","gold","gold","gold","gold","goldenrod4","gold","steelblue","steelblue4","steelblue")
degrees = c(2,10,2,2,10,2,2,2,10,2,2,2,10,2,2,2,2,10,2,2,10,2,2,2,2,2,10,2,2,10,2) #tamanho dos nodos
values = data1$value
arclist=c(1:23)
lablecolor=c(rep("darkred",3),rep("darkred",4),rep("darkred",4),rep("steelblue4",5),rep("goldenrod4",3),rep("goldenrod4",6),rep("goldenrod4",3),rep("steelblue4",3))
#linenodes=c(-10,-17,-17.7,-10,-15.4,-24.7,-17.7,-10,-20.2,-19.7,-20,-10,-23,-21.5,-10,-19.2,-17.9,-19.1,-19,-22.9,-10,-22,-16.8,-10,-25.8,-18.2,-10,-23,-20,-22.3,-24)
linenodes=c(-18,-25,-25.7,-18,-23.4,-32.7,-25.7,-18,-28.2,-27.7,-28,-18,-31,-29.5,-18,-27.2,-25.9,-27.1,-27,-30.9,-18,-30,-24.8,-18,-33.8,-26.2,-18,-31,-28,-30.3,-32)

# plot arc diagram
#dev.new(width=15, height=14)
arcplot(edgelist, ordering=new_ord, cex.labels = 0.8,
        show.nodes = TRUE, cex.nodes = log(degrees) + 0.5, pch.nodes = 21, lwd.nodes = 2, line = linenodes,col.nodes = vborders,
        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values,bg.nodes = vfill,above=arclist,col.labels=lablecolor,horizontal=FALSE) #col.nodes = vborders,, labels = vlabels,
legend("bottomleft",c("Writing Skills","Regulated motivation","Autonomous motivation"),pch=rep('O',2),col=c("darkred","goldenrod4","steelblue4"),box.lty=0)
legend("bottomright",c("Themes","Sub-themes"),lty=rep(1,2),lwd=c(10.5,3),col=c("grey","grey"),box.lty=0)
#locator()
