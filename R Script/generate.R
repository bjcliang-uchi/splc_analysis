sup = data[c(3,1)]
cus = data[c(1,5)]
colnames(sup)=c('from','to')
colnames(cus)=c('from','to')
total=rbind(sup,cus)

net <- graph_from_data_frame(d=total, directed=T) 
plot(net,edge.arrow.size=.4, vertex.size=3, vertex.label=NA,
     layout=layout_on_grid(net))