library(readxl)
library(stringr)
library(dplyr)

Edges_Data <- read_excel("~/Desktop/SPLC/SPLC_SAIC_061515.xlsx", sheet = "Edges")
# Edges Data
Edges_Data$from <- str_replace_all(Edges_Data$Supplier," ","_")
Edges_Data$to <- str_replace_all(Edges_Data$Customer," ","_")
names(Edges_Data)[names(Edges_Data)=="Relationship value (USD)"] = "R"
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
Edges_Data[,1:3]=NULL

#Nodes Data
Nodes_Data <- read_excel("~/Desktop/SPLC/SPLC_SAIC_061515.xlsx", sheet = "Nodes")
Nodes_Data$id <- str_replace_all(Nodes_Data$Company," ","_")
Nodes_Data$Company=NULL
names(Nodes_Data)[names(Nodes_Data)=="Industry Group (GICS)"] = "type_label"
Nodes_Data=Nodes_Data[c('id',"country","type_label","Type_color")]

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

#remove duplicate
Nodes_Data=Nodes_Data[!duplicated(Nodes_Data),]
Edges_Data=Edges_Data[!duplicated(Edges_Data),]

#force factorize
Edges_Data$weight=as.numeric(Edges_Data$weight)
weight = Edges_Data$weight
Edges_Data$weight[is.na(weight)]=min(weight[!is.na(weight)]) #forcing NA to be min
Edges_Data$weight = as.numeric(cut(Edges_Data$weight, breaks=10))

Nodes_Data$type_label=as.factor(Nodes_Data$type_label)
Nodes_Data$country=as.factor(Nodes_Data$country)

Nodes_Data$country_coded="OT"
Nodes_Data$country_coded[which(Nodes_Data$country=='CN')]='CN'
Nodes_Data$country_coded[which(Nodes_Data$country=='DE')]='DE'
Nodes_Data$country_coded[which(Nodes_Data$country=='JP')]='JP'
Nodes_Data$country_coded[which(Nodes_Data$country=='US')]='US'
Nodes_Data$country_coded=as.factor(Nodes_Data$country_coded)

remove(com, id)
remove(weight)

color=Nodes_Data$Type_color
China=as.numeric(color[Nodes_Data$country_coded=='CN'])
print(sd(China))
US=as.numeric(color[Nodes_Data$country_coded=='US'])
print(sd(US))

