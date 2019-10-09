library('xlsx')
library(readxl)
library(stringr)
library(dplyr)
library("igraph")
library("RColorBrewer")

### Read in Data
setwd("~/Desktop/SPLC/data/190821")
data <- read.xlsx("Huawei.xlsx", 2)

data$com_N=as.character(data$com_N)
data$sup_N=as.character(data$sup_N)
data$cus_N=as.character(data$cus_N)
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

Nodes_Data_HW = Nodes
Edges_Data_HW = Edges
remove(Nodes, Edges)

table(Nodes_Data_HW$type_label)
#Nodes_Data_HW$id[which(Nodes_Data_HW$type_label=="#N/A Invalid Security",)]

#### FOR HUAWEI
type0=c("#N/A N/A")
type1=c("Materials","Energy","Utilities")
type2=c("Technology Hardware & Equipmen","Semiconductors & Semiconductor",
        "Capital Goods")
type3=c("Software & Services","Telecommunication Services",
        "Consumer Durables & Apparel")
type4=c("Automobiles & Components","transportation",
        "Commercial & Professional Serv","Household & Personal Products",
        "Health Care Equipment & Servic","Consumer Services",
        "Retailing")
type5=c("Insurance","Media & Entertainment",
        "Food & Staples Retailing",
        "Food, Beverage & Tobacco","Real Estate","Banks",
        "Diversified Financials")


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

net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=F) 

###
x=betweenness(net,normalized = T)
x=x[order(x)]

between_index=data.frame(names(x),as.numeric(x))
colnames(between_index)=c('id','val')
between_index$country=NULL
Nodes_Data$country=as.character(Nodes_Data$country)
for(i in 1:nrow(between_index)){
  between_index$country[i]=Nodes_Data$country[which(between_index$id[i]==Nodes_Data$id)]
}
between_index$country[which(between_index$country=="HK")]="CN"
country_list=between_index$country[!duplicated(between_index$country)]

country_between = data.frame(country_list)
country_between$sum=NA
country_between$num=NA

for(i in 1:length(country_list)){
  country_between$sum[i]=sum(between_index$val[which(between_index$country==country_list[i])])
  country_between$num[i]=length(which(between_index$country==country_list[i]))
}
country_between$avg=country_between$sum/country_between$num
country_between=country_between[order(country_between$sum),]
country_between=country_between[order(country_between$avg),]


# Color by industry sectors
coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Purples") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[V(net)$id==3] = "red"
colx=coul
coul[4]="red"

###########################
# Mark country #
###########################
id_CN=Nodes_Data$id[which(Nodes_Data$country=='CN')]
id_KR=Nodes_Data$id[which(Nodes_Data$country=='KR')]
id_US=Nodes_Data$id[which(Nodes_Data$country=='US')]
id_JP=Nodes_Data$id[which(Nodes_Data$country=='JP')]
id_DE=Nodes_Data$id[which(Nodes_Data$country=='DE')]

l <- layout_with_lgl(net)
net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=T) 
l <- layout_components(net)

par(mfrow=c(2,3), mar=c(1,1,1,1))
plot(net, layout=l, edge.arrow.size=.2,main='CN',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$name,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_CN, mark.col="yellow", 
     mark.border=NA)
#legend('topleft', legend=levels(Nodes_Data$Type_color), 
#       pch=16, col=coul)

plot(net, layout=l, edge.arrow.size=.2,main='US',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$country,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_US, mark.col="yellow", 
     mark.border=NA)
#legend('topleft', legend=levels(Nodes_Data$Type_color), 
#       pch=16, col=coul)

plot(net, layout=l, edge.arrow.size=.2,main='JP',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$country,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_JP, mark.col="yellow", 
     mark.border=NA)
#legend('topleft', legend=levels(Nodes_Data$Type_color), 
#       pch=16, col=coul)

id=Nodes_Data$id
id[1:length(id)]=NA
id[which(Nodes_Data$id=="Kia_Motors_Corp")]="Kia_Motors_Corp"

plot(net, layout=l, edge.arrow.size=.2,main='KR',
     vertex.color=my_color, 
     #vertex.label=NA,
     vertex.label=id,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_KR, mark.col="yellow", 
     mark.border=NA)
#legend('topleft', legend=levels(Nodes_Data$Type_color), 
#       pch=16, col=coul)

plot(net, layout=l, edge.arrow.size=.2,main='DE',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$country,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_DE, mark.col="yellow", 
     mark.border=NA)
#legend('topleft', legend=levels(Nodes_Data$Type_color), 
#       pch=16, col=coul)


between_index$id=as.character(between_index$id)
id_high_btc=between_index$id[225:244]
plot(net, layout=l, edge.arrow.size=.2,main='top20 betweenness centrality',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_high_btc, mark.col="yellow", 
     mark.border=NA)
#legend('topleft', legend=levels(Nodes_Data$Type_color), 
#       pch=16, col=coul)

par(mfrow=c(3,2), mar=c(1,1,1,1))
wide=70
height=0.3
plot(between_index$val[which(between_index$country=="CN")],xlim=c(1,wide),ylim=c(0,height),main = "CN")
plot(between_index$val[which(between_index$country=="US")],xlim=c(1,wide),ylim=c(0,height),main = "US")
plot(between_index$val[which(between_index$country=="JP")],xlim=c(1,wide),ylim=c(0,height),main = "JP")
plot(between_index$val[which(between_index$country=="KR")],xlim=c(1,wide),ylim=c(0,height),main = "KR")
plot(between_index$val[which(between_index$country=="DE")],xlim=c(1,wide),ylim=c(0,height),main = "DE")
plot(between_index$val,main = "total")

