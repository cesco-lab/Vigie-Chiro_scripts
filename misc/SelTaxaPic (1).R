library(data.table)
Today=182

DataF=list.files("./VigieChiro/gbifData/DateSp",full.names=T)
my.data=list()
for (i in 1:length(DataF))
{
  my.data[[i]]=fread(DataF[i])
}
DataDate=rbindlist(my.data)

DataSel=subset(DataDate,(DataDate$PicSp>Today-15)&(DataDate$PicSp<Today+15))


DataSel[sample(nrow(DataSel),1),]

