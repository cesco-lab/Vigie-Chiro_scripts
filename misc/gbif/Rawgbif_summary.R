library(data.table)


DirDataGroup="C:/Users/ybas/Documents/VigieChiro/gbifData/RawTest"
PhylumList=c("Chordata","Arthropoda","Mollusca","Tracheophyta"
             ,"Basidiomycota")


ListGroup=list.files(DirDataGroup
                      ,full.names=T,pattern="_simplified.csv$")


for (h in 1:length(ListGroup)){
  
  DataMod=fread(ListGroup[h])
  DataMod=subset(DataMod,DataMod$phylum %in% PhylumList)
  #table(DataMod$phylum)
  DataMod$precision=ceiling(log(DataMod$coordinateUncertaintyInMeters,10))
  DataMod$precision=pmin(4,DataMod$precision)
  DataMod$precision=pmax(1,DataMod$precision)
  DataMod$precision=ifelse(is.na(DataMod$precision),5,DataMod$precision)
  #table(DataMod$precision)
  test=subset(DataMod,DataMod$countryCode=="IQ")
  Agg1=aggregate(DataMod$eventDate,by=c(list(DataMod$phylum)
                                        ,list(DataMod$countryCode)
                                        ,list(DataMod$precision)
                                        ,list(DataMod$species)),
                                        length)
  Agg2=aggregate(Agg1$x,by=c(list(Agg1$Group.1),list(Agg1$Group.2)
                             ,list(Agg1$Group.3))
                 ,FUN=function(x) round(median(x)))
  # 
  fwrite(DataGroupS,gsub(".csv","_summary.csv",ListGroup[h]),sep=";")
}
