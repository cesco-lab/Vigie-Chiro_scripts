library(data.table)


DirDataGroup="C:/Users/ybas/Documents/VigieChiro/gbifData/RawTest"
PhylumList=c("Chordata","Arthropoda","Mollusca","Tracheophyta"
             ,"Basidiomycota")


ListGroup=list.files(DirDataGroup
                     ,full.names=T,pattern="_simplified.csv$")


for (h in 1:length(ListGroup)){
  DataMod=fread(ListGroup[h])
  DataSummary=fread(gsub("simplified.csv","simplified_summary.csv"
                         ,ListGroup[h]))
  DataMod=subset(DataMod,DataMod$phylum %in% PhylumList)
  #table(DataMod$phylum)
  DataMod$precision=ceiling(log(DataMod$coordinateUncertaintyInMeters,10))
  DataMod$precision=pmin(4,DataMod$precision)
  DataMod$precision=pmax(1,DataMod$precision)
  DataMod$precision=ifelse(is.na(DataMod$precision),5,DataMod$precision)
  
   
  fwrite(Agg2,gsub(".csv","_summary.csv",ListGroup[h]),sep=";")
}
