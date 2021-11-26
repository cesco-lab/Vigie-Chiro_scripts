library(data.table)
DGlist=list.files("./VigieChiro/gbifData/DataGroup",full.names=T)
DaySel=118

DGd=list()
for (i in 1:length(DGlist))
{
  DGd[[i]]=fread(DGlist[i])
  if(i%%100==1){print(i)}
}
DataAll=rbindlist(DGd,use.names=T,fill=T)
DataAll=subset(DataAll,!is.na(DataAll$decimalLatitude))
DataAll$yday=(DataAll$month-1)*30+DataAll$day
DataSel=subset(DataAll,(DataAll$yday>DaySel-15)&(DataAll$yday<DaySel+15))
CoordAll=unique(DataSel,by=c("decimalLatitude","decimalLongitude"))
fwrite(CoordAll,paste0("CoordAll_",DaySel,".csv"),sep=";")

       