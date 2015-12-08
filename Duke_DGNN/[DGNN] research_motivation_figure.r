# install devtools
install.packages("devtools")

# load devtools
library(devtools)

# install arcdiagram
install_github('arcdiagram',  username='gastonstat')

# load arcdiagram
library(arcdiagram)

# location of 'gml' file
mis_file = "/home/joao/Desktop/lesmiserables.txt"

# read 'gml' file
mis_graph = read.graph(mis_file, format = "gml")

# get edgelist
edgelist = get.edgelist(mis_graph)

# get vertex labels
vlabels = get.vertex.attribute(mis_graph, "label")

# get vertex groups
vgroups = get.vertex.attribute(mis_graph, "group")

# get vertex fill color
vfill = get.vertex.attribute(mis_graph, "fill")

# get vertex border color
vborders = get.vertex.attribute(mis_graph, "border")

# get vertex degree
degrees = degree(mis_graph)

# get edges value
values = get.edge.attribute(mis_graph, "value")

# load reshape
library(reshape)

# data frame with vgroups, degree, vlabels and ind
x = data.frame(vgroups, degrees, vlabels, ind = 1:vcount(mis_graph))

# arranging by vgroups and degrees
y = arrange(x, desc(vgroups), desc(degrees))

# get ordering 'ind'

data1<-read.csv("/home/joao/Desktop/data1.csv")
data2<-read.csv("/home/joao/Desktop/data2.csv")

#data1[,1]<-as.character(data1[,1])
#data1[,2]<-as.character(data1[,2])

edgelist = as.matrix(data1[,1:2]) #lista de entrada e saida
new_ord = data2$id #ordem dos itens na figura
vlabels = data2$labels #label dos nodos
vborders = ?? #cor das bordas
vfill = data2$color #cores dos nodos
degrees = data2$degrees #tamanho dos nodos
values = data1$value

# plot arc diagram
arcplot(edgelist, sorted=TRUE, labels = vlabels, cex.labels = 0.8,
        show.nodes = TRUE, bg.nodes = vfill,
        cex.nodes = log(degrees) + 0.5, pch.nodes = 21, lwd.nodes = 2, line = -0.5,
        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values) #col.nodes = vborders,

lab = rbind(c("Emilia", "Kirk"), c("Emilia", "Yong"), c("Filipe", "Matteo"),
c("Filipe", "Tyler"), c("Matteo", "Filipe"), c("Matteo", "Tyler"), c("Mehmet",
"Rori"), c("Rori", "Kirk"), c("Rori", "Vitor"), c("Anna", "Mehmet"),
c("Anna", "Yong"))

arcplot(edgelist)