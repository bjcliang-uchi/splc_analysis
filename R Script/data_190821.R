library('xlsx')
library(readxl)
library(stringr)
library(dplyr)
library("igraph")
library("RColorBrewer")

### Read in Data
data <- read.xlsx("Volkswagen.xlsx", 2)
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

# Color by industry sectors
coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Purples") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[V(net)$Type_color==3] = "red"
coul[4]="red"

#l=layout_with_fr(net,dim=2, niter=nrow(Nodes_Data_HW))
l <- layout_with_graphopt(net, charge=0.00000001)
V(net)$size <- 3.5
#l=layout_with_lgl(net,maxiter=500)
plot(net, layout=l, edge.arrow.size=.2,main='GSC by Sector',
     vertex.label=NA, vertex.color=my_color,vertex.frame.color=my_color)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)

########
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(5,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout,vertex.label=NA, vertex.color=my_color) }
#########

###########################
# Mark country #
###########################
id_CN=Nodes_Data$id[which(Nodes_Data$country=='CN')]
id_KR=Nodes_Data$id[which(Nodes_Data$country=='KR')]
id_US=Nodes_Data$id[which(Nodes_Data$country=='US')]
id_JP=Nodes_Data$id[which(Nodes_Data$country=='JP')]
id_DE=Nodes_Data$id[which(Nodes_Data$country=='DE')]
plot(net, layout=l, edge.arrow.size=.2,main='Volkswagen GSC by Sector (DE Marked), 2019',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$country,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_DE, mark.col="khaki2", 
     mark.border=NA)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)

plot(net, layout=l, edge.arrow.size=.2,main='Volkswagen GSC by Sector (US Marked), 2019',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$country,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_US, mark.col="steelblue1", 
     mark.border=NA)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)


######################################################
######################################################
######################################################
#############  计算 #############
######################################################
######################################################
######################################################

###计算离散度
a=get.vertex.attribute(net, 'country')
m_CN=shortest.paths(net, v=V(net)[a=='CN'])
m_KR=shortest.paths(net, v=V(net)[a=='KR'])
m_US=shortest.paths(net, v=V(net)[a=='US'])
m_JP=shortest.paths(net, v=V(net)[a=='JP'])
m_DE=shortest.paths(net, v=V(net)[a=='DE'])
mean(m_CN)
mean(m_US)
mean(m_KR)
mean(m_JP)
mean(m_DE)

color=Nodes_Data$Type_color
China=as.numeric(color[Nodes_Data$country=='CN'])
print(sd(China))
US=as.numeric(color[Nodes_Data$country=='US'])
print(sd(US))
KR=as.numeric(color[Nodes_Data$country=='KR'])
print(sd(KR))
JP=as.numeric(color[Nodes_Data$country=='JP'])
print(sd(JP))
DE=as.numeric(color[Nodes_Data$country=='DE'])
print(sd(DE))
######


Nodes_Data_HW$country=as.character(Nodes_Data_HW$country)
s_c_country=Edges_Data_HW
node=Nodes_Data_HW
for(i in 1:nrow(node)){
  if(node$country[i]=="HK"){
    node$country[i]="CN"
  }
}
for(i in 1:nrow(s_c_country)){
  s_c_country$s_country[i]=node$country[which(node$id==s_c_country$from[i])]
  s_c_country$c_country[i]=node$country[which(node$id==s_c_country$to[i])]
}

s_c_country$is_same=ifelse(s_c_country$s_country==s_c_country$c_country,1,0)
s_c_country$is_same_CN=ifelse(s_c_country$is_same==1&s_c_country$s_country=="DE",1,0)

sum(s_c_country$is_same)/nrow(s_c_country)
sum(s_c_country$is_same_CN)/nrow(s_c_country)

list_s=s_c_country$from[which(s_c_country$s_country=="DE")]
list_c=s_c_country$to[which(s_c_country$c_country=="DE")]
list=c(list_s,list_c)
list=unique(list) #100

list_list_s=s_c_country$from[which(s_c_country$is_same_CN==1)]
list_list_c=s_c_country$to[which(s_c_country$is_same_CN==1)]
list_list=c(list_list_s,list_list_c)
list_list=unique(list_list) #93

length(list)
length(list_list)

remove(list,list_1,list__c,list__s,list_list_c,list_list_s)

s_c_country$is_same=ifelse(s_c_country$s_country==s_c_country$c_country,1,0)
s_c_country$is_same_US=ifelse(s_c_country$is_same==1&s_c_country$s_country=="US",1,0)

sum(s_c_country$is_same)/nrow(s_c_country) #0.4591121
sum(s_c_country$is_same_US)/nrow(s_c_country) #0.09345794

list_US_s=s_c_country$from[which(s_c_country$s_country=="US")]
list_US_c=s_c_country$to[which(s_c_country$c_country=="US")]
list_US=c(list_US_s,list_US_c)
list_US=unique(list_US) #121

list_US_US_s=s_c_country$from[which(s_c_country$is_same_US==1)]
list_US_US_c=s_c_country$to[which(s_c_country$is_same_US==1)]
list_US_US=c(list_US_US_s,list_US_US_c)
list_US_US=unique(list_US_US) #66

length(list_US)
length(list_US_US)

##### Color by countries

### factorize
Nodes_Data_HW$country_c = as.character(Nodes_Data_HW$country)
keep_country=c("CN","DE","JP","US","KR","OT","GR")
for(i in 1:nrow(Nodes_Data_HW)){
  if(Nodes_Data_HW$country_c[i]=="HK"){
    Nodes_Data_HW$country_c[i]="CN"
  }
  if(Nodes_Data_HW$country_c[i] %in% keep_country){}
  else{
    Nodes_Data_HW$country_c[i]="OT"
  }
}
table(Nodes_Data_HW$country_c)
Nodes_Data_HW$country_c=as.factor(Nodes_Data_HW$country_c)

coul = brewer.pal(nlevels(Nodes_Data_HW$country_c), "Set1") 
my_color=coul[as.numeric(as.factor(Nodes_Data_HW$country_c))]
#plot(net, layout=l, edge.arrow.size=.4, vertex.label=Nodes_Data$lab, vertex.color=my_color, main='GSC by Country',vertex.label.dist=pi/2)
plot(net, layout=l, edge.arrow.size=.2, vertex.label=NA, vertex.color=my_color, main='Volkswagen GSC by Country')
legend('topleft', legend=levels(Nodes_Data_HW$country_c), 
       pch=16, col=coul)

