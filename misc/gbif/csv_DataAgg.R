library(data.table)

DirCSVs="./www/CSVs"

LF=list.files(DirCSVs,full.names=T)

dataF=list()
#for (i in 1:length(LF))
for (i in 49:64)
{
  dataF[[i]]=fread(LF[i])
  print(LF[i])
  dataF[[i]]=dataF[[i]][order(dataF[[i]]$PMaxtot,decreasing=T),]
print(head(dataF[[i]],50))
  }
DataAll=rbindlist(dataF)

DataAgg=aggregate(DataAll$PMaxtot,by=list(DataAll$EspNew),max)

fwrite(DataAgg,"DataAgg.csv",sep=";")
fwrite(DataAll,"DataAll.csv",sep=";")
