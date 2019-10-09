country_edge = s_c_country[c(4,5)]
colnames(country_edge)=c('from', 'to')
country_edge$weight = 1

country_edge=country_edge[which(country_edge$from != country_edge$to),]

country_list=Nodes_Data$country[!duplicated(Nodes_Data$country)]
country_node = data.frame(country_list)
country_node$size = 2

edge_color = rep("lightgrey", nrow(country_edge))
edge_color[which(country_edge$from=="CN" | country_edge$to == "CN")] = 'red'

net_country <- graph_from_data_frame(d=country_edge, vertices=country_list, directed=F) 
l=layout_as_star(net_country, center=V(net_country)[which(country_list=='CN')])

library(colorRamps)
coul = blue2red(nlevels(country_node$country_list)) 
coul = rep("white", 33)
plot(net_country, layout=l, edge.arrow.size=.2,main='Country Network',
     edge.color=edge_color, 
     vertex.label=country_node$country_list, 
     vertex.color=coul,vertex.frame.color=coul)

library(maps)
library(geosphere)
library(ggmap)
concap = read.csv("/Users/chen.liang/Downloads/concap.csv")

concap$CountryCode=as.character(concap$CountryCode)
country_list=as.character(country_node$country_list)

country_node$latitude=NA
country_node$longitude=NA
for(i in 1:33){
  country_node$latitude[i] = concap$CapitalLatitude[which(concap$CountryCode==country_node$country_list[i])]
  country_node$longitude[i] = concap$CapitalLongitude[which(concap$CountryCode==country_node$country_list[i])]
}
