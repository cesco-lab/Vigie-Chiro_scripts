library(data.table)
library(readxl)
LDG=list.files("./VigieChiro/gbifData/DataGroup",full.names=T)
GroupSp=read_excel("GroupeSp.xlsx")

LDG_G=tstrsplit(LDG,split="_")[[2]]

for (i in 1:nrow(GroupSp))
{
  LDGi=subset(LDG,LDG_G==GroupSp$Group[i])
  if(length(LDGi))
  {
  LDGl=list()
  for (j in 1:length(LDGi))
  {
    LDGl[[j]]=fread(LDGi[j])
  }
  DataG=rbindlist(LDGl,use.names=T,fill=T)
  DataG=subset(DataG,!is.na(DataG$decimalLatitude))
if(length(unique(DataG$name))<10)
   {print(GroupSp$Group[i])}
}
}
