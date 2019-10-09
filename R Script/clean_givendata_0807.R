setwd("~/Desktop/SPLC/data")
library('xlsx')
library(readxl)
library(stringr)
library(dplyr)
library("igraph")
library("RColorBrewer")

# Read in Data
sup <- read.xlsx("Huawei20190807.xlsx", 3)
cus <- read.xlsx("Huawei20190807.xlsx", 4)

### Nodes_Data
node1=data.frame('id'=c(1:nrow(sup)))
node2=data.frame('id'=c(1:nrow(cus)))
node1$id=sup$suppliersName
node1$country=sup$Country
node1$type_label=sup$GicsIndustryGroupName
node2$id=cus$customerName
node2$country=cus$Country
node2$type_label=cus$GicsIndustryGroupName
Nodes_Data_HW=rbind(node1, node2)
Nodes_Data_HW$id=as.character(Nodes_Data_HW$id)
Nodes_Data_HW$country=as.character(Nodes_Data_HW$country)
Nodes_Data_HW$type_label=as.character(Nodes_Data_HW$type_label)
Nodes_Data_HW=Nodes_Data_HW[!is.na(Nodes_Data_HW$id),]
Nodes_Data_HW=Nodes_Data_HW[Nodes_Data_HW$id!="",]
Nodes_Data_HW=Nodes_Data_HW[Nodes_Data_HW$id!="#N/A Invalid Security",]

### Edges_Data
sup1=sup[c(2,4,8)]
cus1=cus[c(2,4,8)]
colnames(sup1)=c("to","from","weight")
colnames(cus1)=c("from","to","weight")
Edges_Data_HW=rbind(cus1, sup1)
Edges_Data_HW=Edges_Data_HW[!is.na(Edges_Data_HW$from),]
Edges_Data_HW=Edges_Data_HW[Edges_Data_HW$from!="",]
Edges_Data_HW=Edges_Data_HW[Edges_Data_HW$from!="#N/A Invalid Security",]
Edges_Data_HW=Edges_Data_HW[!is.na(Edges_Data_HW$to),]
Edges_Data_HW=Edges_Data_HW[Edges_Data_HW$to!="",]
Edges_Data_HW=Edges_Data_HW[Edges_Data_HW$to!="#N/A Invalid Security",]

remove(cus,cus1,node1,node2,sup,sup1)

# Rename and Combine
Edges_Data_HW$from <- str_replace_all(Edges_Data_HW$from," ","_")
Edges_Data_HW$to <- str_replace_all(Edges_Data_HW$to," ","_")
Nodes_Data_HW$id <- str_replace_all(Nodes_Data_HW$id," ","_")
Edges_Data_HW$weight=as.numeric(Edges_Data_HW$weight)

#remove duplicate
Nodes_Data_HW=Nodes_Data_HW[!duplicated(Nodes_Data_HW),]
Edges_Data_HW=Edges_Data_HW[!duplicated(Edges_Data_HW),]

#check complete
for(com in Edges_Data_HW$from){
  if(!(com %in% Nodes_Data_HW$id)){
    print(com)
  }
}
for(com in Edges_Data_HW$to){
  if(!(com %in% Nodes_Data_HW$id)){
    print(com)
  }
}
remove(com)

### factorize
keep_country=c("CN","DE","JP","US","KR","OT")
for(i in 1:nrow(Nodes_Data_HW)){
  if(Nodes_Data_HW$country[i] %in% keep_country){}
  if(Nodes_Data_HW$country[i]=="HK"){
    Nodes_Data_HW$country[i]=="CN"
  }
  else{
    Nodes_Data_HW$country[i]="OT"
  }
}
Nodes_Data_HW$country=as.factor(Nodes_Data_HW$country)

#table(Nodes_Data_HW$type_label)
#Nodes_Data_HW$id[which(Nodes_Data_HW$type_label=="Utilities",)]
type0=c("#N/A N/A")
type1=c("Materials","Energy","Utilities")
type2=c("Technology Hardware & Equipmen","Semiconductors & Semiconductor",
        "Capital Goods")
type3=c("Software & Services","Telecommunication Services",
        "Consumer Durables & Apparel")
type4=c("Automobiles & Components","transportation",
        "Commercial & Professional Serv","Household & Personal Products",
        "Health Care Equipment & Servic","Consumer Services")
type5=c("Insurance","Media & Entertainment",
        "Retailing","Food & Staples Retailing",
        "Food, Beverage & Tobacco","Real Estate","Banks",
        "Diversified Financials")
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

remove(keep_country,i,type0,type1,type2,type3,type4,type5)

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
id_US=Nodes_Data$id[which(Nodes_Data$country=='US')]
plot(net, layout=l, edge.arrow.size=.2,main='Huawei GSC by Sector (China Marked), 2019',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.label=Nodes_Data$country,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_CN, mark.col="khaki2", 
     mark.border=NA)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)

plot(net, layout=l, edge.arrow.size=.2,main='GSC by Sector (US Marked), 2019',
     vertex.color=my_color, 
     vertex.label=NA,
     #vertex.label=Nodes_Data$country_coded,
     mark.groups = id_US, mark.col="steelblue1", mark.border=NA)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)


# Color by countries
coul = brewer.pal(nlevels(Nodes_Data$country), "Set1") 
my_color=coul[as.numeric(as.factor(Nodes_Data$country))]
#plot(net, layout=l, edge.arrow.size=.4, vertex.label=Nodes_Data$lab, vertex.color=my_color, main='GSC by Country',vertex.label.dist=pi/2)
plot(net, layout=l, edge.arrow.size=.2, vertex.label=NA, vertex.color=my_color, main='Huawei GSC by Country')
legend('topleft', legend=levels(Nodes_Data$country), 
       pch=16, col=coul)

sapply(Edges_Data, class)
sapply(Nodes_Data, class)


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
m_US=shortest.paths(net, v=V(net)[a=='US'])
mean(m_CN)
mean(m_US)

color=Nodes_Data$Type_color
China=as.numeric(color[Nodes_Data$country=='CN'])
print(sd(China))
US=as.numeric(color[Nodes_Data$country=='US'])
print(sd(US))
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
s_c_country$is_same_CN=ifelse(s_c_country$is_same==1&s_c_country$s_country=="CN",1,0)

sum(s_c_country$is_same)/nrow(s_c_country)
sum(s_c_country$is_same_CN)/nrow(s_c_country)

list_CN_s=s_c_country$from[which(s_c_country$s_country=="CN")]
list_CN_c=s_c_country$to[which(s_c_country$c_country=="CN")]
list_CN=c(list_CN_s,list_CN_c)
list_CN=unique(list_CN) #100

list_CN_CN_s=s_c_country$from[which(s_c_country$is_same_CN==1)]
list_CN_CN_c=s_c_country$to[which(s_c_country$is_same_CN==1)]
list_CN_CN=c(list_CN_CN_s,list_CN_CN_c)
list_CN_CN=unique(list_CN_CN) #93

remove(list_CN,list_CN_1,list_CN_CN_c,list_CN_CN_s,list_CN_c,list_CN_s)

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