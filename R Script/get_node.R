
library(xlsx)
library(dplyr)
path = "/Users/chen.liang/Desktop/SPLC/data/All\ Phones.xlsx"
samsung = read.xlsx(path, "samsung")
xiaomi = read.xlsx(path, "xiaomi")
apple = read.xlsx(path, "AAPL")
huawei = read.xlsx(path, "KMCACZ CH Equity")

data = rbind(samsung,xiaomi, apple)
data = data %>% mutate_at(vars(com, sup, cus), as.character)
all_companies_ticker = unique(c(data$com, data$sup, data$cus))
access_com_info = data.frame("ticker" = all_companies_ticker)
access_com_info = access_com_info %>% filter(ticker!= 0 &
                                             ticker!= "#N/A N/A")

get_Name="=IF(ISBLANK(A2),\"\",BDP(A2, \"LONG_COMP_NAME\",\"\"))"
get_country="=IF(ISBLANK(A2),\"\",BDP(A2, \"CNTRY_OF_DOMICILE\",\"\"))"
get_gics="=IF(ISBLANK(A2),\"\",BDP(A2, \"GICS_INDUSTRY_GROUP_NAME\",\"\"))"


access_com_info$Name = NA
access_com_info$country = NA
access_com_info$gics = NA
for(i in 1:(nrow(access_com_info))){
  company = paste0("A", i+1)
  access_com_info$Name[i] = gsub("A2",company, get_Name)
  access_com_info$country[i] = gsub("A2",company, get_country)
  access_com_info$gics[i] = gsub("A2",company, get_gics)
}

