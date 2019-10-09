library("network")
library("RColorBrewer")

Nodes_Data$country_coded="OT"
Nodes_Data$country_coded[which(Nodes_Data$country=='CN')]='CN'
Nodes_Data$country_coded[which(Nodes_Data$country=='DE')]='DE'
Nodes_Data$country_coded[which(Nodes_Data$country=='JP')]='JP'
Nodes_Data$country_coded[which(Nodes_Data$country=='US')]='US'
Nodes_Data$country_coded=as.integer(as.factor(Nodes_Data$country_coded))
Nodes_Data=Nodes_Data[c('id',"country","country_coded","type_label","size")]
Nodes_Data$country=as.character(Nodes_Data$country)
Nodes_Data$type_label=as.character(Nodes_Data$type_label)

net2=network(Edges_Data, vertex.attr = Nodes_Data, matrix.type="edgelist",loops=T, multiple=T, ignore.eval = F)
net2 %v% 'type_label'

coul = brewer.pal(nlevels(as.factor(Nodes_Data$type_label)), "Set1") 
net2 %v% 'col'= coul[as.numeric(as.factor(Nodes_Data$type_label))]
Nodes_Data$x=NA
a=which(Nodes_Data$id=="SAIC_Motor_Corp_Ltd")
Nodes_Data$x[a]=Nodes_Data$id[a]

plot(net2, vertex.cex=(net2 %v% "size")/5, vertex.col="col",
     label=Nodes_Data$x)
legend('topleft', legend=levels(as.factor(Nodes_Data$type_label)), 
       pch=16, col=coul)


