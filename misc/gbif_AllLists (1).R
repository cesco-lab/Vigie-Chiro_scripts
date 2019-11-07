library(readxl)
f_gbif_listsp="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/gbif_listsp.R"
GroupList=read_excel("GroupeSp.xlsx")

source(f_gbif_listsp)

for (i in 8:nrow(GroupList))
{
  print(GroupList$Group[i])
  gbif_listsp(countrylist=c("France","Spain","Italy","Switzerland")
              ,rank=GroupList$Rank[i]
              ,group=GroupList$Group[i])
}
