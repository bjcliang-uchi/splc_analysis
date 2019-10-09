library('xlsx')
library(readxl)
library(stringr)
library(dplyr)
library("igraph")
library("RColorBrewer")

### Read in Data
setwd("~/Desktop/SPLC/data/190821")
vk <- read.xlsx("Volkswagen.xlsx", 2)
gm <- read.xlsx("GM.xlsx", 2)
Hy <- read.xlsx("Hyundai.xlsx", 2)
ty <- read.xlsx("Toyota.xlsx", 2)
sa <- read.xlsx("SAIC.xlsx", 2)
data=rbind(vk,gm)
data=rbind(data,Hy)
data=rbind(data,ty)
data=rbind(data,sa)
remove(vk,gm,Hy,ty,sa)

lg=555
data$com_N=as.character(data$com_N)
data$sup_N=as.character(data$sup_N)
data$cus_N=as.character(data$cus_N)

list_vk=c(data$com_N[1:lg],data$sup_N[1:lg],data$cus_N[1:lg])
list_gm=c(data$com_N[(lg+1):(2*lg)],data$sup_N[(lg+1):(2*lg)],data$cus_N[(lg+1):(2*lg)])
list_hy=c(data$com_N[(2*lg+1):(3*lg)],data$sup_N[(2*lg+1):(3*lg)],data$cus_N[(2*lg+1):(3*lg)])
list_ty=c(data$com_N[(3*lg+1):(4*lg)],data$sup_N[(3*lg+1):(4*lg)],data$cus_N[(3*lg+1):(4*lg)])
list_sa=c(data$com_N[(4*lg+1):(5*lg)],data$sup_N[(4*lg+1):(5*lg)],data$cus_N[(4*lg+1):(5*lg)])
remove(lg)

list_vk=list_vk[!is.na(list_vk) & list_vk!="" & list_vk!="#N/A Invalid Security"]
list_gm=list_gm[!is.na(list_gm) & list_gm!="" & list_gm!="#N/A Invalid Security"]
list_hy=list_hy[!is.na(list_hy) & list_hy!="" & list_hy!="#N/A Invalid Security"]
list_ty=list_ty[!is.na(list_ty) & list_ty!="" & list_ty!="#N/A Invalid Security"]
list_sa=list_sa[!is.na(list_sa) & list_sa!="" & list_sa!="#N/A Invalid Security"]
list_vk <- str_replace_all(list_vk," ","_")
list_vk=list_vk[!duplicated(list_vk)]
list_gm <- str_replace_all(list_gm," ","_")
list_gm=list_gm[!duplicated(list_gm)]
list_hy <- str_replace_all(list_hy," ","_")
list_hy=list_hy[!duplicated(list_hy)]
list_ty <- str_replace_all(list_ty," ","_")
list_ty=list_ty[!duplicated(list_ty)]
list_sa <- str_replace_all(list_sa," ","_")
list_sa=list_sa[!duplicated(list_sa)]

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

Nodes_Data_HW = Nodes
Edges_Data_HW = Edges
remove(Nodes, Edges)

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

net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=F) 

###
x=betweenness(net,normalized = T)
x=x[order(x)]
x[510:500]
between_index=data.frame(names(x),as.numeric(x))
colnames(between_index)=c('id','val')
between_index$country=NULL
Nodes_Data$country=as.character(Nodes_Data$country)
for(i in 1:nrow(between_index)){
  between_index$country[i]=Nodes_Data$country[which(between_index$id[i]==Nodes_Data$id)]
}

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

par(mfrow=c(3,2), mar=c(1,1,1,1))
plot(between_index$val[which(between_index$country=="CN")],xlim=c(1,130),ylim=c(0,0.12),main = "CN")
plot(between_index$val[which(between_index$country=="US")],xlim=c(1,130),ylim=c(0,0.12),main = "US")
plot(between_index$val[which(between_index$country=="JP")],xlim=c(1,130),ylim=c(0,0.12),main = "JP")
plot(between_index$val[which(between_index$country=="KR")],xlim=c(1,130),ylim=c(0,0.12),main = "KR")
plot(between_index$val[which(between_index$country=="DE")],xlim=c(1,130),ylim=c(0,0.12),main = "DE")
plot(between_index$val,main = "total")

# Color by industry sectors
coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Purples") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[V(net)$id==3] = "red"
colx=coul
coul[4]="red"

#l=layout_with_fr(net,dim=2, niter=nrow(Nodes_Data_HW))
l <- layout_with_graphopt(net, charge=0.00000001)
V(net)$size <- 3.5
#l=layout_with_lgl(net,maxiter=500)
plot(net, layout=l, edge.arrow.size=.2,main='GSC by Sector',
     vertex.label=NA, vertex.color=my_color,vertex.frame.color=my_color)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)



#######
# Mark specific company network
a=Nodes_Data$id %in% list_gm
b=Nodes_Data$id %in% list_hy
c=Nodes_Data$id %in% list_sa
d=Nodes_Data$id %in% list_ty
e=Nodes_Data$id %in% list_vk
is_gm = which(a)
is_hy = which(b)
is_sa = which(c)
is_ty = which(d)
is_vk = which(e)

remove(a,b,c,d,e)

h=c(1:nrow(Nodes_Data))
h[1:nrow(Nodes_Data)]=0
h[is_gm]=h[is_gm]+1
h[is_hy]=h[is_hy]+1
h[is_sa]=h[is_sa]+1
h[is_ty]=h[is_ty]+1
h[is_vk]=h[is_vk]+1
is_high_freq=which(h>=4)
Nodes_Data$country=as.character(Nodes_Data$country)
table(Nodes_Data$country[is_high_freq])
Nodes_Data$type_label=as.character(Nodes_Data$type_label)
table(Nodes_Data$type_label[is_high_freq])

high_freq=Nodes_Data[is_high_freq,]

hf_CN=high_freq$id[which(high_freq$country=="CN")]
i=5
hf_CN[i]
hf_CN[i] %in% list_gm
hf_CN[i] %in% list_hy
hf_CN[i] %in% list_sa
hf_CN[i] %in% list_ty
hf_CN[i] %in% list_vk


coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Purples") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[is_gm]='red'
plot(net, layout=l, edge.arrow.size=.2,main='GSC by Sector, GM marked',
     vertex.label=NA, vertex.color=my_color,vertex.frame.color=my_color)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)

my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[is_vk]='red'
plot(net, layout=l, edge.arrow.size=.2,main='GSC by Sector, VK marked',
     vertex.label=NA, vertex.color=my_color,vertex.frame.color=my_color)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)


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
id_high_btc=between_index$id[481:510]
plot(net, layout=l, edge.arrow.size=.2,main='top40 betweenness centrality',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightblue",
     mark.groups = id_high_btc, mark.col="yellow", 
     mark.border=NA)
#legend('topleft', legend=levels(Nodes_Data$Type_color), 
#       pch=16, col=coul)



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


Nodes_Data$country=as.character(Nodes_Data$country)
s_c_country=Edges_Data
node=Nodes_Data
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
Nodes_Data$country_c = as.character(Nodes_Data$country)
keep_country=c("CN","DE","JP","US","KR","OT","GR")
for(i in 1:nrow(Nodes_Data)){
  if(Nodes_Data$country_c[i]=="HK"){
    Nodes_Data$country_c[i]="CN"
  }
  if(Nodes_Data$country_c[i] %in% keep_country){}
  else{
    Nodes_Data$country_c[i]="OT"
  }
}
table(Nodes_Data$country_c)
Nodes_Data$country_c=as.factor(Nodes_Data$country_c)

coul = brewer.pal(nlevels(Nodes_Data$country_c), "Set1") 
my_color=coul[as.numeric(as.factor(Nodes_Data$country_c))]
#plot(net, layout=l, edge.arrow.size=.4, vertex.label=Nodes_Data$lab, vertex.color=my_color, main='GSC by Country',vertex.label.dist=pi/2)
plot(net, layout=l, edge.arrow.size=.2, vertex.label=NA, vertex.color=my_color, main='Volkswagen GSC by Country')
legend('topleft', legend=levels(Nodes_Data$country_c), 
       pch=16, col=coul)

net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=F) 
shortest_paths(net, from="Xiaomi_Corp", to="Toyota_Motor_Corp",mode='all')
shortest_paths(net, to="Xiaomi_Corp", from="Toyota_Motor_Corp",mode='all')

x=all_shortest_paths(net, from="Volkswagen_AG", to="Semiconductor_Manufacturing_International_Corp",mode='all')
y=names(x$res[[1]])

for(i in 1:length(x$res)){
  print(names(x$res[[i]]))
}

a=which(Edges_Data$from=="Volkswagen_AG" & Edges_Data$to=="Semiconductor_Manufacturing_International_Corp")
if(length(a)==0){
  print("<-")
}
if(length(a)!=0){
  print('->')
}

897677





