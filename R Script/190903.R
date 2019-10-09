library(xlsx)
#library(readxl)
library(stringr)
library(dplyr)
library("igraph")
library("RColorBrewer")

### Read in Data
setwd("~/Desktop/SPLC/data/190831")
amat = read.xlsx("da_equipment.xlsx", 1)
asml = read.xlsx("da_equipment.xlsx", 2)
lrcx = read.xlsx("da_equipment.xlsx", 3)
tkye = read.xlsx("da_equipment.xlsx", 4)
niko = read.xlsx("da_equipment.xlsx", 5)
cano = read.xlsx("da_equipment.xlsx", 6)

amat[]=lapply(amat,type.convert,as.is=T)
asml[]=lapply(asml,type.convert,as.is=T)
lrcx[]=lapply(lrcx,type.convert,as.is=T)
tkye[]=lapply(tkye,type.convert,as.is=T)
niko[]=lapply(niko,type.convert,as.is=T)
cano[]=lapply(cano,type.convert,as.is=T)

company_set=data.frame(c(amat$com_N, amat$sup_N, amat$cus_N), 
                       c(asml$com_N, asml$sup_N, asml$cus_N),
                       c(lrcx$com_N, lrcx$sup_N, lrcx$cus_N),
                       c(tkye$com_N, tkye$sup_N, tkye$cus_N),
                       c(niko$com_N, niko$sup_N, niko$cus_N),
                       c(cano$com_N, cano$sup_N, cano$cus_N))
colnames(company_set) = c('amat','asml','lrcx','tkye','niko','cano')
equipment=do.call('rbind',list(amat,asml,lrcx,tkye,niko,cano))
remove(amat,asml,lrcx,tkye,niko,cano)

silt = read.xlsx("da_materials.xlsx", 1)
sksl = read.xlsx("da_materials.xlsx", 2)
shin = read.xlsx("da_materials.xlsx", 3)
sumc = read.xlsx("da_materials.xlsx", 4)
wafe = read.xlsx("da_materials.xlsx", 5)
silt[]=lapply(silt,type.convert,as.is=T)
sksl[]=lapply(sksl,type.convert,as.is=T)
shin[]=lapply(shin,type.convert,as.is=T)
sumc[]=lapply(sumc,type.convert,as.is=T)
wafe[]=lapply(wafe,type.convert,as.is=T)
company_set$silt=c(silt$com_N, silt$sup_N, silt$cus_N)
company_set$sksl=c(sksl$com_N, sksl$sup_N, sksl$cus_N)
company_set$shin=c(shin$com_N, shin$sup_N, shin$cus_N)
company_set$sumc=c(sumc$com_N, sumc$sup_N, sumc$cus_N)
company_set$wafe=c(wafe$com_N, wafe$sup_N, wafe$cus_N)
material=do.call('rbind',list(silt,sksl,shin,sumc,wafe))
remove(silt,sksl,shin,sumc,wafe)

sams = read.xlsx("da_storage.xlsx", 1)
skhy = read.xlsx("da_storage.xlsx", 2)
micr = read.xlsx("da_storage.xlsx", 3)
tosh = read.xlsx("da_storage.xlsx", 4)
digi = read.xlsx("da_storage.xlsx", 5)
sams[]=lapply(sams,type.convert,as.is=T)
skhy[]=lapply(skhy,type.convert,as.is=T)
micr[]=lapply(micr,type.convert,as.is=T)
tosh[]=lapply(tosh,type.convert,as.is=T)
digi[]=lapply(digi,type.convert,as.is=T)
company_set$sams=c(sams$com_N, sams$sup_N, sams$cus_N)
company_set$skhy=c(skhy$com_N, skhy$sup_N, skhy$cus_N)
company_set$micr=c(micr$com_N, micr$sup_N, micr$cus_N)
company_set$tosh=c(tosh$com_N, tosh$sup_N, tosh$cus_N)
company_set$digi=c(digi$com_N, digi$sup_N, digi$cus_N)
storage=do.call('rbind',list(sams,skhy,micr,tosh,digi))
remove(sams,skhy,micr,tosh,digi)

inte = read.xlsx("da_yuanqi.xlsx", 1)
admi = read.xlsx("da_yuanqi.xlsx", 2)
nvid = read.xlsx("da_yuanqi.xlsx", 3)
tsmc = read.xlsx("da_yuanqi.xlsx", 4)
glob = read.xlsx("da_yuanqi.xlsx", 5)
unmi = read.xlsx("da_yuanqi.xlsx", 6)
smic = read.xlsx("da_yuanqi.xlsx", 7)
inte[]=lapply(inte,type.convert,as.is=T)
admi[]=lapply(admi,type.convert,as.is=T)
nvid[]=lapply(nvid,type.convert,as.is=T)
tsmc[]=lapply(tsmc,type.convert,as.is=T)
glob[]=lapply(glob,type.convert,as.is=T)
unmi[]=lapply(unmi,type.convert,as.is=T)
smic[]=lapply(smic,type.convert,as.is=T)
company_set$inte=c(inte$com_N, inte$sup_N, inte$cus_N)
company_set$admi=c(admi$com_N, admi$sup_N, admi$cus_N)
company_set$nvid=c(nvid$com_N, nvid$sup_N, nvid$cus_N)
company_set$tsmc=c(tsmc$com_N, tsmc$sup_N, tsmc$cus_N)
company_set$glob=c(glob$com_N, glob$sup_N, glob$cus_N)
company_set$unmi=c(unmi$com_N, unmi$sup_N, unmi$cus_N)
company_set$smic=c(smic$com_N, smic$sup_N, smic$cus_N)
yuanqi=do.call('rbind',list(inte,admi,nvid,tsmc,glob,unmi,smic))
remove(inte,admi,nvid,tsmc,glob,unmi,smic)

ls_yuanqi=c(yuanqi$com_N, yuanqi$sup_N, yuanqi$cus_N)
ls_equipment=c(equipment$com_N, equipment$sup_N, equipment$cus_N)
ls_storage=c(storage$com_N, storage$sup_N, storage$cus_N)
ls_material=c(material$com_N, material$sup_N, material$cus_N)
data=do.call('rbind',list(equipment,material,storage,yuanqi))
remove(equipment,material,storage,yuanqi)

x=company_set
for(i in names(company_set)){
  company_set[,i][which(company_set[,i]=="#N/A Invalid Security"|company_set[,i]=="")]=NA
  company_set[,i][duplicated(company_set[,i])]=NA
}
remove(i)

supply = data[c(2,4,7)]
cus = data[c(6,2,8)]
colnames(supply)=c("to","from","weight")
colnames(cus)=c("to","from","weight")
Edges_Data = rbind(supply, cus)
remove(cus, supply)

#com is not included, so probably error occurs
supply_n = data[c(4,9,10)]
cus_n = data[c(6,11,12)]
colnames(supply_n)=c("id","country","type_label")
colnames(cus_n)=c("id","country","type_label")
Nodes_Data = rbind(supply_n, cus_n)
remove(supply_n, cus_n)

Nodes_Data$country=as.character(Nodes_Data$country)
Nodes_Data$country[which(Nodes_Data$country=="HK")]="CN"

### Clean Data
Edges_Data=Edges_Data[!is.na(Edges_Data$from) & Edges_Data$from!="" & Edges_Data$from!="#N/A Invalid Security",]
Edges_Data=Edges_Data[!is.na(Edges_Data$to) & Edges_Data$to!="" & Edges_Data$to!="#N/A Invalid Security",]
Edges_Data=Edges_Data[!duplicated(Edges_Data),]

Nodes_Data=Nodes_Data[!is.na(Nodes_Data$id) & Nodes_Data$id!="" & Nodes_Data$id!="#N/A Invalid Security",]
Nodes_Data=Nodes_Data[!duplicated(Nodes_Data),]

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
Nodes_Data=rbind(Nodes_Data, c('SK Siltron Co Ltd','KR','Materials'))
remove(com)

table(Nodes_Data$type_label)
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
for(i in 1:nrow(Nodes_Data)){
  if(Nodes_Data$type_label[i] %in% type0){
    Nodes_Data$Type_color[i]=0
  }
  if(Nodes_Data$type_label[i] %in% type1){
    Nodes_Data$Type_color[i]=1
  }
  if(Nodes_Data$type_label[i] %in% type2){
    Nodes_Data$Type_color[i]=2
  }
  if(Nodes_Data$type_label[i] %in% type3){
    Nodes_Data$Type_color[i]=3
  }
  if(Nodes_Data$type_label[i] %in% type4){
    Nodes_Data$Type_color[i]=4
  }
  if(Nodes_Data$type_label[i] %in% type5){
    Nodes_Data$Type_color[i]=5
  }
}
table(Nodes_Data$Type_color)

remove(i,type0,type1,type2,type3,type4,type5)

####################################################
####################################################
####################################################
### Making Network #################################
####################################################
####################################################
####################################################

Nodes_Data$size=3.5
Edges_Data$weight=1
Nodes_Data$country=as.factor(Nodes_Data$country)
Nodes_Data$Type_color=as.factor(Nodes_Data$Type_color)
net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=T) 

####################################################
### Calculate Betweenness ##########################
####################################################
x=betweenness(net,normalized = T)
x=x[order(x)]
between_index=data.frame(names(x),as.numeric(x))
colnames(between_index)=c('id','val')
between_index$country=NULL
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
remove(country_list)
country_between$avg=country_between$sum/country_between$num
country_between=country_between[order(country_between$sum),]
country_between=country_between[order(country_between$avg),]

par(mfrow=c(3,2), mar=c(1,1,1,1))
xl=300
yl=0.03
plot(between_index$val[which(between_index$country=="CN")],xlim=c(1,xl),ylim=c(0,yl),main = "CN")
plot(between_index$val[which(between_index$country=="US")],xlim=c(1,xl),ylim=c(0,yl),main = "US")
plot(between_index$val[which(between_index$country=="JP")],xlim=c(1,xl),ylim=c(0,yl),main = "JP")
plot(between_index$val[which(between_index$country=="KR")],xlim=c(1,xl),ylim=c(0,yl),main = "KR")
plot(between_index$val[which(between_index$country=="DE")],xlim=c(1,xl),ylim=c(0,yl),main = "DE")
plot(between_index$val,main = "total")
remove(country_list,xl,yl,i,x)

####################################################
####################################################
####################################################

cut = Nodes_Data$id[as.logical(degree(net)==1)]
net2=delete_vertices(net,cut)
l=layout_with_lgl(net)

my_color=rep('lightgrey',sum(degree(net)!=1))
my_color[V(net2)$country=='CN']='red'

plot(net, layout=l, edge.arrow.size=.2,main='CN',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightgrey")

my_color=rep('grey', nrow(Nodes_Data))
my_color[V(net)$country=='CN']='red'
l=layout_with_lgl(net)
V(net)$size = 2
plot(net, layout=l, edge.arrow.size=.2,main='CN',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightyellow")


####################################################
####################################################
####################################################

coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Purples") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
V(net)$size <- 3
l=layout_with_lgl(net)
plot(net, layout=l, edge.arrow.size=.1, main='GSC by Sector',
     vertex.label=NA, edge.color="lightyellow",
     vertex.color=my_color, vertex.frame.color=my_color)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)


# Color by industry sectors
coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Purples") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
V(net)$size <- 3
l=layout_with_lgl(net)
plot(net, layout=l, edge.arrow.size=.1, main='GSC by Sector',
     vertex.label=NA, edge.color="lightyellow",
     vertex.color=my_color, vertex.frame.color=my_color)
legend('topleft', legend=levels(Nodes_Data$Type_color), 
       pch=16, col=coul)

###########################
# Mark country #
###########################
l <- layout_with_lgl(net)
net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=T) 

par(mfrow=c(2,3), mar=c(1,1,1,1))
V(net)$size=2

my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[V(net)$country == 'CN'] = "red"
plot(net, layout=l, edge.arrow.size=.2,main='CN',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightyellow")

my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[V(net)$country == 'US'] = "red"
plot(net, layout=l, edge.arrow.size=.2,main='US',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightyellow")

my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[V(net)$country == 'JP'] = "red"
plot(net, layout=l, edge.arrow.size=.2,main='JP',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightyellow")

my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[V(net)$country == 'KR'] = "red"
plot(net, layout=l, edge.arrow.size=.2,main='KR',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightyellow")

my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[V(net)$country == 'DE'] = "red"
plot(net, layout=l, edge.arrow.size=.2,main='DE',
     vertex.color=my_color, 
     vertex.label=NA,
     vertex.frame.color=my_color,
     edge.color="lightyellow")

between_index$id=as.character(between_index$id)
id_high_btc=between_index$id[1000:1185]
coul = brewer.pal(nlevels(Nodes_Data$Type_color), "Purples") 
my_color=coul[as.numeric(as.factor(Nodes_Data$Type_color))]
my_color[names(V(net)) %in% id_high_btc] = "red"
plot(net, 
     layout=l, edge.arrow.size=.2,main='top betweenness centrality',
     vertex.color=my_color, vertex.frame.color=my_color,
     vertex.label=NA,
     edge.color="lightyellow",
     mark.border=NA)









#centr_eigen
# Visualizing network structure
V(net)$coreness <- coreness(net)
par(mfrow=c(2, 3), mar=c(0.1,0.1,1,0.1))
set.seed(777); fr <- layout_with_fr(net)
for (k in 1:6){
  V(net)$color <- ifelse(V(net)$coreness>=k, "orange", "grey")
  plot(net, main=paste0(k, '-core shell'), layout=fr, 
       vertex.label=NA,
       vertex.frame.color=V(net)$color,
       edge.color="lightgrey")
}

k=6
V(net)$color <- ifelse(V(net)$coreness>=k, "orange", "white")
plot(net, main=paste0(k, '-core shell'), layout=fr, 
     vertex.label=NA,
     vertex.frame.color=V(net)$color,
     edge.color="grey")





k=6
cut = Nodes_Data$id[as.logical(V(net)$coreness<k)]
net2=delete_vertices(net,cut)

names=names(V(net2))
x=which(!(Edges_Data$from %in% names))
y=which(!(Edges_Data$to %in% names))
cut=c(x,y)
cut=cut[!duplicated(cut)]
edge_cutted=Edges_Data[-cut,]
remove(x,y,cut)

node_cutted=Nodes_Data[which(Nodes_Data$id %in% names),]
net3 <- graph_from_data_frame(d=edge_cutted, vertices=node_cutted, directed=T) 


V(net3)$size=2

plot(net3, 
     layout=fr, 
     vertex.label=NA,
     vertex.color="orange",
     vertex.frame.color="orange",
     edge.arrow.size=.2,
     edge.color="grey")


