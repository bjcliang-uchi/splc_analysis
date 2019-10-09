library("igraph")
library("RColorBrewer")

Nodes_Data$country=as.factor(Nodes_Data$country)
Nodes_Data$Type_color=as.factor(Nodes_Data$Type_color)
Nodes_Data$size=5
#Nodes_Data$size=Nodes_Data$size/1.5

Edges_Data$weight=1
net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=T) 
l <- layout_with_lgl(net)

###计算离散度

a=get.vertex.attribute(net, 'country_coded')
m_CN=shortest.paths(net, v=V(net)[a=='CN'])
m_US=shortest.paths(net, v=V(net)[a=='US'])
mean(m_CN)
mean(m_US)
######

# Color by industry sectors
coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Greys") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]

my_color[V(net)$Type_color==3] = "red"
coul[4]="red"

plot(net, layout=l, edge.arrow.size=.4,main='GSC by Sector',
     vertex.label=NA, vertex.color=my_color)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)

###########################
# Mark country #
###########################
id_CN=Nodes_Data$id[which(Nodes_Data$country_coded=='CN')]
id_US=Nodes_Data$id[which(Nodes_Data$country_coded=='US')]
plot(net, layout=l, edge.arrow.size=.3,main='GSC by Sector (China Marked), 061519',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$id,
     mark.groups = id_CN, mark.col="#ECD89A", mark.border=NA)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)

plot(net, layout=l, edge.arrow.size=.3,main='GSC by Sector (US Marked), 061519',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$country_coded,
     mark.groups = id_US, mark.col="steelblue1", mark.border=NA)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)

###########################
# Mark SAIC #
###########################
Nodes_Data$x=NA
Nodes_Data$x[1]=substr(Nodes_Data$id[1],1,4)
plot(net, layout=l, edge.arrow.size=.3,main='GSC by Sector and Country',
     vertex.color=my_color, 
     vertex.label=Nodes_Data$x,
     mark.groups = id_CN, mark.col="#ECD89A", mark.border=NA)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)
###########################

# Color by countries
coul = brewer.pal(nlevels(Nodes_Data$country_coded), "Set1") 
my_color=coul[as.numeric(as.factor(Nodes_Data$country_coded))]
#plot(net, layout=l, edge.arrow.size=.4, vertex.label=Nodes_Data$lab, vertex.color=my_color, main='GSC by Country',vertex.label.dist=pi/2)
plot(net, layout=l, edge.arrow.size=.4, vertex.label=NA, vertex.color=my_color, main='GSC by Country')
legend('topleft', legend=levels(Nodes_Data$country_coded), 
       pch=16, col=coul)

