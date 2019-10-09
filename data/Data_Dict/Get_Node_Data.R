setwd("~/Desktop/变量/SPLC/data")
library(xlsx)
library(dplyr)

data1 = read.xlsx("da_equipment.xlsx", "Sheet7")
data2 = read.xlsx("da_equipment.xlsx", "Sheet8")
data3 = read.xlsx("da_equipment.xlsx", "Sheet9")
data4 = read.xlsx("da_equipment.xlsx", "Sheet10")
data5 = read.xlsx("da_equipment.xlsx", "Sheet11")
data6 = read.xlsx("da_equipment.xlsx", "Sheet12")
data7 = read.xlsx("da_materials.xlsx", "Sheet6")
data8 = read.xlsx("da_materials.xlsx", "Sheet7")
data9 = read.xlsx("da_materials.xlsx", "Sheet8")
data10 = read.xlsx("da_materials.xlsx", "Sheet9")
data11 = read.xlsx("da_materials.xlsx", "Sheet10")
data12 = read.xlsx("da_storage.xlsx", "Sheet1")
data13 = read.xlsx("da_storage.xlsx", "Sheet2")
data14 = read.xlsx("da_storage.xlsx", "Sheet3")
data15 = read.xlsx("da_storage.xlsx", "Sheet4")
data16 = read.xlsx("da_storage.xlsx", "Sheet5")
data17 = read.xlsx("da_yuanqi.xlsx", "Sheet1")
data18 = read.xlsx("da_yuanqi.xlsx", "Sheet2")
data19 = read.xlsx("da_yuanqi.xlsx", "Sheet3")
data20 = read.xlsx("da_yuanqi.xlsx", "Sheet4")
data21 = read.xlsx("da_yuanqi.xlsx", "Sheet5")
data22 = read.xlsx("da_yuanqi.xlsx", "Sheet6")
data23 = read.xlsx("da_yuanqi.xlsx", "Sheet7")

data = rbind(data1, data2, data3, data4, data5, data6,
             data7, data8, data9, data10, data11, data12,
             data13, data14, data15, data16, data17, data18,
             data19, data20, data21, data22, data23)


data = data %>% mutate_all(as.character)
supply_n = data[c(3,4,9,10)]
cus_n = data[c(5,6,11,12)]
colnames(supply_n)=c("ticker","id","country","type_label")
colnames(cus_n)=c("ticker","id","country","type_label")
Nodes_Data = rbind(supply_n, cus_n)
remove(supply_n, cus_n)
Nodes_Data=Nodes_Data[!is.na(Nodes_Data$id) & Nodes_Data$id!="" & Nodes_Data$id!="#N/A Invalid Security",]
Nodes_Data=Nodes_Data[!duplicated(Nodes_Data),]

write.csv(Nodes_Data, "Data_Dict/Nodes_Data.csv", row.names = F)


