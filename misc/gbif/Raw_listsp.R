library(data.table)
library(stringr)

DirDataGroup="C:/Users/ybas/Documents/VigieChiro/gbifData/RawTest"
#Groupes=fread("C:/Users/ybas/Documents/GroupeSp2.csv")
#CoefMedian=2
DateSel=fread("C:/Users/ybas/Documents/VigieChiro/gbifData/DateSel.csv")

ListGroup=list.files(DirDataGroup
                     ,full.names=T,pattern="_simplified_shortened.csv$")
ListGroup=subset(ListGroup,substr(basename(ListGroup),1,3)!="GI_")
print(ListGroup)
# Groupes$Rank=tolower(Groupes$Rank)
# Groupes$Group=str_to_title(Groupes$Group)
# table(Groupes$Rank)

DataList=list()
for (h in 1:length(ListGroup)){
  DataMod=fread(ListGroup[h])
  DataListh=unique(DataMod,by=c("species"))
  DataList[[h]]=DataListh
  print(nrow(DataListh))
}
DataListAll=rbindlist(DataList,fill=T,use.names=T)
DataListAll=unique(DataListAll,by="species")
fwrite(DataListAll,paste0(dirname(DirDataGroup),"/DataListAll.csv")
       ,sep=";")
