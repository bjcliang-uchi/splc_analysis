
Node_Code = Nodes_Data[1]
Node_Code$Code = 1:nrow(Nodes_Data)

Edge_export = Edges_Data[1:2]
for(i in 1:nrow(Edge_export)){
  Edge_export$to[i] = Node_Code$Code[which(Node_Code$id==Edge_export$to[i])]
  Edge_export$from[i] = Node_Code$Code[which(Node_Code$id==Edge_export$from[i])]
}

write.table(Edge_export, "export.txt", sep="\t", row.names = F, col.names = F)
Node_Code = Node_Code[,c(2,1)]

Node_Code$size=3
Edges_Data$weight=1
net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=T) 
l=layout_with_graphopt(net)

plot(net, layout=l, edge.arrow.size=.2,
     edge.color="lightgrey", vertex.label=NA)

result = sort(page_rank(net, damping = 0.85, directed = T)$vector)
result = data.frame(names(result), as.numeric(result))
colnames(result) = c('id', 'pagerank')

result$type = 1:nrow(result)
for(i in 1:nrow(result)){
  result$type[i] = Nodes_Data$Type_color[which(Nodes_Data$id==result$id[i])]
}
plot(result$type, result$pagerank)


result2 = sort(authority_score(net)$vector)
result2 = data.frame(names(result2), as.numeric(result2))
colnames(result2) = c('id', 'authority')

result2$type = 1:nrow(result2)
result2$id = as.character(result2$id)
Nodes_Data$Type_color = as.numeric(Nodes_Data$Type_color)
for(i in 1:nrow(result2)){
  print(Nodes_Data$Type_color[which(Nodes_Data$id==result2$id[i])])
  result2$type[i] = Nodes_Data$Type_color[which(Nodes_Data$id==result2$id[i])]
}
plot(result2$type, result2$authority)

