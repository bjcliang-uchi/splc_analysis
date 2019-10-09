setwd("~/Desktop/SPLC")
library('xlsx')
library(readxl)
library(stringr)
library(dplyr)
library("igraph")
library("RColorBrewer")

# Read in Data
deep1 <- read.xlsx("SPLC_TOYOTA_250715.xlsx", 1)
deep1 = deep1[1:3]
deep2 <- read.xlsx("SPLC_TOYOTA_250715.xlsx", 2)
deep2 = deep2[1:3]
deep3 <- read.xlsx("SPLC_TOYOTA_250715.xlsx", 3, header=FALSE)
deep3 = deep3[1:3]
colnames(deep3) = c('supplier','Customer','Value')
Nodes_Data <- read.xlsx("SPLC_TOYOTA_250715.xlsx", 4)
Nodes_Data=Nodes_Data[1:2]

# Rename and Combine
Edges_Data = rbind(deep1, deep2)
Edges_Data = rbind(Edges_Data, deep3)
Edges_Data = Edges_Data[!is.na(Edges_Data$supplier) & !is.na(Edges_Data$Customer) ,]
remove(deep1, deep2, deep3)
colnames(Edges_Data)=c('from','to','R')

Edges_Data$from <- str_replace_all(Edges_Data$from," ","_")
Edges_Data$to <- str_replace_all(Edges_Data$to," ","_")
Nodes_Data$id <- str_replace_all(Nodes_Data$company," ","_")
Nodes_Data=Nodes_Data[c('id',"country")]

#change weight
Edges_Data$R=as.character(Edges_Data$R)
Edges_Data$weight = substr(Edges_Data$R,1,nchar(Edges_Data$R)-1)
for (id in 1:length(Edges_Data$R)){
  if(grepl('K',Edges_Data$R[id])){
    Edges_Data$weight[id] = as.numeric(Edges_Data$weight[id])*1000
  }
  if(grepl('M',Edges_Data$R[id])){
    Edges_Data$weight[id] = as.numeric(Edges_Data$weight[id])*1000000
  }
  if(grepl('B',Edges_Data$R[id])){
    Edges_Data$weight[id] = as.numeric(Edges_Data$weight[id])*1000000000
  }
}
Edges_Data$weight=as.numeric(Edges_Data$weight)
weight = Edges_Data$weight
Edges_Data$weight[is.na(weight)]=min(weight[!is.na(weight)]) #forcing NA to be min
Edges_Data$weight = as.numeric(cut(Edges_Data$weight, breaks=10))
Edges_Data$R = NULL

#remove duplicate
Nodes_Data=Nodes_Data[!duplicated(Nodes_Data),]
Edges_Data=Edges_Data[!duplicated(Edges_Data),]

#check complete
for(com in Edges_Data$from){
  if(!(com %in% Nodes_Data$id)){
    print(com)
  }
}
for(com in Edges_Data$to){
  if(!(com %in% Nodes_Data$id)){
    print(com)
  }
}
remove(com)

Nodes_Data$country_coded="OT"
Nodes_Data$country_coded[which(Nodes_Data$country=='CN')]='CN'
Nodes_Data$country_coded[which(Nodes_Data$country=='DE')]='DE'
Nodes_Data$country_coded[which(Nodes_Data$country=='JP')]='JP'
Nodes_Data$country_coded[which(Nodes_Data$country=='US')]='US'
Nodes_Data$country_coded=as.factor(Nodes_Data$country_coded)


### Making Network
Nodes_Data$size=5
net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=FALSE) 

id_JP=Nodes_Data$id[which(Nodes_Data$country_coded=='JP')]
id_US=Nodes_Data$id[which(Nodes_Data$country_coded=='US')]

plot(net, edge.arrow.size=.3, main='GSC by Sector (JP Marked), 2015',
     #vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$country_coded,
     mark.groups = c(id_JP, id_US), mark.col=c("red", "blue"), mark.border=NA)

sapply(Edges_Data, class)
sapply(Nodes_Data, class)


