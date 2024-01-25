library(readxl)
library(data.table)
f_gbif_listsp="E:/DataGroup/gbif_listsp.R"
GroupList=read_excel("C:/Users/admin/Desktop/GroupeSp.xlsx")
Renew=T
CountryList=c("France","Spain")

if(!Renew)
{
  ListAlready=list.files("./VigieChiro/gbifData/ListSp")
  GroupAlready=tstrsplit(ListAlready,"_")[[2]]
  GroupList=subset(GroupList,!(GroupList$Group %in% GroupAlready))
}

source(f_gbif_listsp)

for (i in 1:nrow(GroupList))
{
  #  print(GroupList$Group[i])
  gbif_listsp(
    countrylist=CountryList
    ,
    rank=GroupList$Rank[i]
    ,
    group=GroupList$Group[i]
  )
}
