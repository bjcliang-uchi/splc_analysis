cus=cus[c(3,5)]
sup=sup[c(3,5)]
colnames(sup)=c("com","country")
colnames(cus)=c("com","country")
list=rbind(cus,sup)
list$country=as.character(list$country)
list=list[!is.na(list$com),]
list=list[list$com!="",]
list=list[list$com!="#N/A N/A",]
list=list[list$com!="#N/A Invalid Security",]
list=list[!duplicated(list),]
list$country[which(list$country=="HK")]="CN"


