library(readxl)
library(data.table)
f_gbif_listsp="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/gbif_listsp.R"
GroupList=read_excel("GroupeSp.xlsx")
Renew=F

if(!Renew)
{
  ListAlready=list.files("./VigieChiro/gbifData/ListSp")
GroupAlready=tstrsplit(ListAlready,"_")[[2]]
GroupList=subset(GroupList,!(GroupList$Group %in% GroupAlready))
}

source(f_gbif_listsp)

for (i in 2:nrow(GroupList))
{
  print(GroupList$Group[i])
  gbif_listsp(
    countrylist=c("France","Spain","Italy","Switzerland")
              ,
    rank=GroupList$Rank[i]
              ,
    group=GroupList$Group[i]
    )
}
