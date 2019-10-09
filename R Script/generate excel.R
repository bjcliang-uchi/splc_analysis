setwd("~/Desktop/SPLC/data")
repeat_t = 5
model=data.frame("company"="A2",
                 "supplier"=NA, "sup_Name"=NA,
                 "customer"=NA, "cus_Name"=NA)

#### Company
a=1+repeat_t+repeat_t^2
for(i in 2:a){
  add=data.frame("company"=paste0('C', i),
                 "supplier"=NA, "sup_Name"=NA,
                 "customer"=NA, "cus_Name"=NA)
  model=rbind(model,add)
}
for(i in 2:a){
  add=data.frame("company"=paste0('E', i),
                 "supplier"=NA, "sup_Name"=NA,
                 "customer"=NA, "cus_Name"=NA)
  model=rbind(model,add)
}

b=a*repeat_t+2
c=b+repeat_t^2-1
for(i in b:c){
  add=data.frame("company"=paste0('C', i),
                 "supplier"=NA, "sup_Name"=NA,
                 "customer"=NA, "cus_Name"=NA)
  model=rbind(model,add)
}
for(i in b:c){
  add=data.frame("company"=paste0('E', i),
                 "supplier"=NA, "sup_Name"=NA,
                 "customer"=NA, "cus_Name"=NA)
  model=rbind(model,add)
}
data=model[rep(seq_len(nrow(model)), each=repeat_t),]
data$company=as.character(data$company)
data$customer=as.character(data$customer)
data$supplier=as.character(data$supplier)
data$cus_Name=as.character(data$cus_Name)
data$sup_Name=as.character(data$sup_Name)


###
seq=seq(1,nrow(data),repeat_t)
for(i in seq){
  data$customer[i]=data$company[i]
  data$supplier[i]=data$company[i]
}
for(i in 1:nrow(data)){
  data$cus_Name[i]=paste0("E",i+1)
  data$sup_Name[i]=paste0("C",i+1)
}

remove(add,model,a,b,c,i,seq)


customer="company"
get_Name="=IF(ISBLANK(A2),\"\",BDP(A2, \"LONG_COMP_NAME\",\"\"))"
get_supplier="=BDS(A2,\"SUPPLY_CHAIN_SUPPLIERS\",\"SUPPLY_CHAIN_SUM_COUNT_OVERRIDE=5,QUANTIFIED_OVERRIDE=Y,SUP_CHAIN_RELATIONSHIP_SORT_OVR=C\",\"cols=1;rows=5\")"
get_customer="=BDS(A2,\"SUPPLY_CHAIN_CUSTOMERS\",\"SUPPLY_CHAIN_SUM_COUNT_OVERRIDE=5,QUANTIFIED_OVERRIDE=Y,SUP_CHAIN_RELATIONSHIP_SORT_OVR=C\",\"cols=1;rows=5\")"

get_relationship_S="=IF(ISBLANK(C2),\"\",BDP(A2, \"RELATIONSHIP_AMOUNT\",\"RELATIONSHIP_OVERRIDE=S,QUANTIFIED_OVERRIDE=Y,EQY_FUND_CRNCY=USD,RELATED_COMPANY_OVERRIDE=\"&C2))"
get_relationship_C="=IF(ISBLANK(C2),\"\",BDP(A2, \"RELATIONSHIP_AMOUNT\",\"RELATIONSHIP_OVERRIDE=C,QUANTIFIED_OVERRIDE=Y,EQY_FUND_CRNCY=USD,RELATED_COMPANY_OVERRIDE=\"&C2))"

get_country="=IF(ISBLANK(A2),\"\",BDP(A2, \"CNTRY_OF_DOMICILE\",\"\"))"
get_gics="=IF(ISBLANK(A2),\"\",BDP(A2, \"GICS_INDUSTRY_GROUP_NAME\",\"\"))"

form = data.frame('com'=NA,'com_N'=NA,
                  'sup'=NA,'sup_N'=NA, 
                  'cus'=NA,'cus_N'=NA,
                  'rel_V_fr_S'=NA,'rel_V_to_C'=NA,
                  'sup_country'=NA,'sup_gics'=NA,
                  'cus_country'=NA,'cus_gics'=NA)

all_N=repeat_t+2*repeat_t^2+4*repeat_t^3
form=form[rep(seq_len(nrow(form)), each=all_N),]

for(i in 1:nrow(data)){
  form$com[i] = paste0("=",data$company[i])
  form$com_N[i]=gsub("A2",data$company[i],get_Name)
  form$sup[i]=gsub("A2",data$supplier[i],get_supplier)
  form$cus[i]=gsub("A2",data$customer[i],get_customer)
  form$sup_N[i]=gsub("A2",data$sup_Name[i],get_Name)
  form$cus_N[i]=gsub("A2",data$cus_Name[i],get_Name)
  
  form$rel_V_fr_S[i]=gsub('A2',paste0("A",i+1),get_relationship_S)
  form$rel_V_fr_S[i]=gsub('C2',data$sup_Name[i],form$rel_V_fr_S[i])
  
  form$rel_V_to_C[i]=gsub('A2',paste0("A",i+1),get_relationship_C)
  form$rel_V_to_C[i]=gsub('C2',data$cus_Name[i],form$rel_V_to_C[i])
  
  form$sup_country[i]=gsub("A2",data$sup_Name[i],get_country)
  form$cus_country[i]=gsub("A2",data$cus_Name[i],get_country)
  form$sup_gics[i]=gsub("A2",data$sup_Name[i],get_gics)
  form$cus_gics[i]=gsub("A2",data$cus_Name[i],get_gics)
}


form$com[1]="company ticker"
write.csv(form, "data_access.csv",row.names=FALSE, na="")