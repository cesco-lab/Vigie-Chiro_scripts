library(data.table)

Dir="F:/DataSM4_LekoUS/Results_Kaleidoscope"

IdFiles=list.files(Dir,pattern="id.csv$",full.names=T,recursive=T)


IdData=list()
for ( i in 1:length(IdFiles)){
  IdData[[i]]=fread(IdFiles[i])
  
}
IdTable=rbindlist(IdData)
table(IdTable$`AUTO ID*`)
boxplot(IdTable$`MATCH RATIO`~IdTable$`AUTO ID*`,las=2)
boxplot(IdTable$MARGIN~IdTable$`AUTO ID*`,las=2)
head(IdTable$`IN FILE`)
tail(IdTable$`IN FILE`)
