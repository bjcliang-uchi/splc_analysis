library('xlsx')
library(readxl)
library(stringr)
library(dplyr)
library("igraph")
library("RColorBrewer")

### Read in Data
setwd("~/Desktop/SPLC/data/190821")
data = read.xlsx("Volkswagen.xlsx", 2)

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

data$com_N=as.character(data$com_N)
data$sup_N=as.character(data$sup_N)
data$cus_N=as.character(data$cus_N)

supply = data[c(2,4,7)]
cus = data[c(6,2,8)]
colnames(supply)=c("to","from","weight")
colnames(cus)=c("to","from","weight")
Edges_Data = rbind(supply, cus)
remove(cus, supply)

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
remove(com)

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


#####################
##### Network #######
#####################

Nodes_Data$Type_color=as.factor(Nodes_Data$Type_color)
Nodes_Data$size=3
Edges_Data$weight=1
net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=T) 
l=layout_with_graphopt(net)


### mark 5 companies
company_5 = c('SAIC Motor Corp Ltd','Hyundai Motor Co',
              'Toyota Motor Corp', 'General Motors Co', 'Volkswagen AG')
my_color=rep('lightgrey',length(Nodes_Data$id))
my_color[Nodes_Data$id %in% company_5]='red'
label = ifelse(Nodes_Data$id %in% company_5, Nodes_Data$id, "")
label[which(label=="SAIC Motor Corp Ltd")]="SAIC"
label[which(label=="Hyundai Motor Co")]="Hyundai"
label[which(label=="Toyota Motor Corp")]="Toyota"
label[which(label=="General Motors Co")]="GM"
label[which(label=="Volkswagen AG")]="VK"

plot(net, layout=l, edge.arrow.size=.2,main='Mark 5 Central Companies',
     edge.color="lightgrey", 
     vertex.label=NA, vertex.color=my_color,vertex.frame.color=my_color)

remove(coul, label, my_color, company_5)

##################
##################
# Probability 
##################

Nodes_Data$country=as.character(Nodes_Data$country)
s_c_country=Edges_Data
node=Nodes_Data

for(i in 1:nrow(s_c_country)){
  s_c_country$s_country[i]=node$country[which(node$id==s_c_country$from[i])]
  s_c_country$c_country[i]=node$country[which(node$id==s_c_country$to[i])]
}
remove(i)

country = "DE"
s_c_country$is_same=ifelse(s_c_country$s_country==s_c_country$c_country,1,0)
s_c_country$is_same_specify=ifelse(s_c_country$is_same==1 &
                                   s_c_country$s_country==country, 1,0)

list_list_s=s_c_country$from[which(s_c_country$is_same_specify==1)]
list_list_c=s_c_country$to[which(s_c_country$is_same_specify==1)]
list_list=c(list_list_s,list_list_c)
list_list=unique(list_list) 
remove(list_list_s,list_list_c)

country_num = sum(Nodes_Data$country==country)
paste0(country, ": ", length(list_list)," / ",country_num,' = ', 
       round(length(list_list)/country_num,4)*100, "%")
remove(country, country_num, list_list)
remove(s_c_country)

##################
##################
### 
##################

lg=555
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
list_vk=list_vk[!duplicated(list_vk)]
list_gm=list_gm[!duplicated(list_gm)]
list_hy=list_hy[!duplicated(list_hy)]
list_ty=list_ty[!duplicated(list_ty)]
list_sa=list_sa[!duplicated(list_sa)]

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
h = rep(0, nrow(Nodes_Data))
h[is_gm]=h[is_gm]+1
h[is_hy]=h[is_hy]+1
h[is_sa]=h[is_sa]+1
h[is_ty]=h[is_ty]+1
h[is_vk]=h[is_vk]+1
all_appear=which(h==5)

my_color=rep('lightgrey',length(Nodes_Data$id))
my_color[all_appear]='red'
plot(net, layout=l, edge.arrow.size=.2,main='Companies Appeared in All 5 Subnets',
     edge.color="lightgrey", 
     vertex.label=NA, vertex.color=my_color,vertex.frame.color=my_color)

country = "DE"
show_country = which(h==5 & Nodes_Data$country == country)
my_color=rep('lightgrey',length(Nodes_Data$id))
my_color[show_country]='red'
plot(net, layout=l, edge.arrow.size=.2,main=country,
     edge.color="lightgrey", 
     vertex.label=NA, vertex.color=my_color,vertex.frame.color=my_color)

remove(is_gm, is_hy, is_sa, is_ty, is_vk)
remove(list_gm, list_hy, list_sa, list_ty, list_vk)
remove(h, show_country, all_appear)
remove(my_color,country)


##################
##################
### Betweenness ##
##################

Nodes_Data$size=3
Edges_Data$weight=1
net <- graph_from_data_frame(d=Edges_Data, vertices=Nodes_Data, directed=F) 
#l=layout_with_graphopt(net)

x=betweenness(net,normalized = T)
#x=rev(x[order(x)])
between_index=data.frame(names(x), round(as.numeric(x),4))
colnames(between_index)=c('Company','BC')
between_index$Country=NULL
for(i in 1:nrow(between_index)){
  between_index$Country[i]=as.character(Nodes_Data$country[which(between_index$Company[i]==Nodes_Data$id)])
}

top10 = as.character(between_index$Company)[1:10]
top10 = which(Nodes_Data$id %in% top10)

my_color=rep('lightgrey',length(Nodes_Data$id))
my_color[top10]='red'
plot(net, layout=l, edge.arrow.size=.2,main="Top 10 Marked",
     edge.color="lightgrey", 
     vertex.label=NA, vertex.color=my_color,vertex.frame.color=my_color)


country_list=between_index$Country[!duplicated(between_index$Country)]
sum=matrix(nrow = length(country_list))
num=matrix(nrow = length(country_list))
for(i in 1:length(country_list)){
  sum[i]=sum(between_index$BC[which(between_index$Country==country_list[i])])
  num[i]=length(which(between_index$Country==country_list[i]))
}

country_BC = data.frame(country_list, sum, num)
country_BC$avg=country_BC$sum/country_BC$num
country_BC=country_BC[order(country_BC$avg),]
reversed = data.frame(rev(country_BC$country_list),
                      rev(country_BC$sum),
                      rev(country_BC$num),
                      round(rev(country_BC$avg),4))
colnames(reversed) = c('Country', "Sum", "Num", "Avg")
remove(num, sum, reversed, country_list, my_color, i, top10, x)

