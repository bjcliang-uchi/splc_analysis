library('xlsx')
library(readxl)
library(stringr)
library(dplyr)
library("igraph")
library("RColorBrewer")

### Read in Data
setwd("~/Desktop/SPLC/data/190821")

#vk <- read.xlsx("Volkswagen.xlsx", 2)
#gm <- read.xlsx("GM.xlsx", 2)
#Hy <- read.xlsx("Hyundai.xlsx", 2)
#ty <- read.xlsx("Toyota.xlsx", 2)
sa <- read.xlsx("SAIC.xlsx", 2)

data=sa

supply = data[c(2,4,7)]
cus = data[c(6,2,8)]
colnames(supply)=c("to","from","weight")
colnames(cus)=c("to","from","weight")
Edges = rbind(supply, cus)
remove(cus, supply)

supply_n = data[c(4,9,10)]
cus_n = data[c(6,11,12)]
colnames(supply_n)=c("id","country","type_label")
colnames(cus_n)=c("id","country","type_label")
Nodes = rbind(supply_n, cus_n)
remove(supply_n, cus_n)

Nodes$country=as.character(Nodes$country)
Nodes$country[which(Nodes$country=="HK")]="CN"

### Clean Data
Edges=Edges[!is.na(Edges$from) & Edges$from!="" & Edges$from!="#N/A Invalid Security",]
Edges=Edges[!is.na(Edges$to) & Edges$to!="" & Edges$to!="#N/A Invalid Security",]
Edges$from <- str_replace_all(Edges$from," ","_")
Edges$to <- str_replace_all(Edges$to," ","_")
Edges=Edges[!duplicated(Edges),]

Nodes=Nodes[!is.na(Nodes$id) & Nodes$id!="" & Nodes$id!="#N/A Invalid Security",]
Nodes$id <- str_replace_all(Nodes$id," ","_")
Nodes=Nodes[!duplicated(Nodes),]

#check complete
for(com in Edges$from){
  if(!(com %in% Nodes$id)){
    print(com)
  }
}
for(com in Edges$to){
  if(!(com %in% Nodes$id)){
    print(com)
  }
}
remove(com)

x=table(Nodes$country)
x=x[order(x)]
x=rev(x)
y=data.frame(names(x), as.numeric(x))
colnames(y)=c('country','num')
y$ratio = paste0(round(y$num/length(Nodes$country),4)*100, '%')


Nodes_Data_HW = Nodes
Edges_Data_HW = Edges
#remove(Nodes, Edges)

table(Nodes_Data_HW$type_label)
#Nodes_Data_HW$id[which(Nodes_Data_HW$type_label=="#N/A Invalid Security",)]

#### FOR Automobiles
type0=c("#N/A N/A")
type1=c("Materials","Energy","Utilities")
type2=c("Technology Hardware & Equipmen","Semiconductors & Semiconductor",
        "Capital Goods")
type3=c("Automobiles & Components")
type4=c("transportation","Software & Services",
        "Consumer Durables & Apparel",
        "Commercial & Professional Serv","Household & Personal Products",
        "Consumer Services","Retailing")
type5=c("Insurance","Media & Entertainment",
        "Food & Staples Retailing",
        "Food, Beverage & Tobacco","Real Estate","Banks",
        "Telecommunication Services",
        "Health Care Equipment & Servic",
        "Diversified Financials")
#####
#####
for(i in 1:nrow(Nodes_Data_HW)){
  if(Nodes_Data_HW$type_label[i] %in% type0){
    Nodes_Data_HW$Type_color[i]=0
  }
  if(Nodes_Data_HW$type_label[i] %in% type1){
    Nodes_Data_HW$Type_color[i]=1
  }
  if(Nodes_Data_HW$type_label[i] %in% type2){
    Nodes_Data_HW$Type_color[i]=2
  }
  if(Nodes_Data_HW$type_label[i] %in% type3){
    Nodes_Data_HW$Type_color[i]=3
  }
  if(Nodes_Data_HW$type_label[i] %in% type4){
    Nodes_Data_HW$Type_color[i]=4
  }
  if(Nodes_Data_HW$type_label[i] %in% type5){
    Nodes_Data_HW$Type_color[i]=5
  }
}
table(Nodes_Data_HW$Type_color)

remove(i,type0,type1,type2,type3,type4,type5)
#remove(lg)
####################################################
####################################################







####################################################
### Making Network #################################
####################################################
####################################################
####################################################
Nodes_Data=Nodes_Data_HW
Edges_Data=Edges_Data_HW

Nodes_Data$size=3.5
Edges_Data$weight=1

Nodes_Data$country=as.factor(Nodes_Data$country)
Nodes_Data$Type_color=as.factor(Nodes_Data$Type_color)

net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=T) 






###########################
# Mark country #
###########################
l <- layout_with_lgl(net)
coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Greys") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[Nodes_Data$country=="CN"]='red'

plot(net, layout=l, edge.arrow.size=.2,main='SAIC',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$name,
     vertex.frame.color=my_color,
     edge.color="lightgrey")
