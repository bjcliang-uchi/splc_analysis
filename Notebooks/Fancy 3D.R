#devtools::install_github('nstrayer/network3d')
library("network3d")
library(dplyr)
library("igraph")
library("RColorBrewer")
setwd('/Users/chen.liang/Desktop/变量/splc_analysis/data')
Nodes_Data = readRDS(file = "Temp/Nodes_Data.rds")
Edges_Data = readRDS(file = "Temp/Edges_Data.rds")

Nodes = Nodes_Data[c(2,5)]
Nodes$Code= 1:nrow(Nodes_Data)
Nodes = Nodes[c(3,1,2)]
colnames(Nodes) = c("id","name","group")

Edges = Edges_Data
for(i in 1:nrow(Edges_Data)){
  Edges$to[i] = Nodes$id[Nodes$name == Edges$to[i]]
  Edges$from[i] = Nodes$id[Nodes$name == Edges$from[i]]
}
colnames(Edges) = c("source","target", "weight")
Nodes$group = as.factor(Nodes$group)
#coul = brewer.pal(nlevels(Nodes$group), "Purples") 
my_color=rep("lightblue",time = nrow(Nodes))
#Nodes = Nodes[c(1,2,4)]

company_id = c('Applied Materials Inc','ASML Holding NV','Lam Research Corp', 
               'Tokyo Electron Ltd', 'Nikon Corp', 'Canon Inc', 
               'Siltronic AG', 'SK Siltron Co Ltd', 'Shin-Etsu Chemical Co Ltd', 
               'SUMCO Corp','Globalwafers Co Ltd',
               'Samsung Electronics Co Ltd', 'SK Hynix Inc', 'Micron Technology Inc',
               'Toshiba Corp','Western Digital Corp',
               'Intel Corp','Advanced Micro Devices Inc', 'NVIDIA Corp',
               'Taiwan Semiconductor Manufacturing Co Ltd','GLOBALFOUNDRIES Inc', 
               'United Microelectronics Corp','Semiconductor Manufacturing International Corp',
               "Huawei Investment & Holding Co Ltd")

my_color[Nodes$name %in% company_id]='yellow'
my_color[Nodes$name == "Huawei Investment & Holding Co Ltd"]='red'

intera = rep(FALSE, time = nrow(Nodes))
intera[Nodes$name %in% company_id]=TRUE

colnames(Nodes) = c("id","tooltip", "Notes")
Nodes$selectable = intera
Nodes$color = my_color

Nodes$size = 0.03
Nodes$size[Nodes$tooltip %in% company_id] = 0.15
network3d(Nodes, Edges, node_size = Nodes$size,
          node_outline_black = F,
          edge_opacity = 0.25, background_color = "black",raycast_res=0.05)




