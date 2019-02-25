library(data.table)

Occurences=fread("./VigieChiro/Traits/GBIF/OccSL_bird.csv")
SpTarget="Fulcri"
Sample=1000


OccP=subset(Occurences,Occurences$Esp==SpTarget)
if(nrow(OccP)>Sample)
{
  OccP=OccP[sample.int(nrow(OccP),size=Sample),]
}
OccA=subset(Occurences,Occurences$Esp!=SpTarget)
OccA=OccA[sample.int(nrow(OccA),size=Sample),]
OccP$presence=1
plot(OccP$decimalLongitude,OccP$decimalLatitude)
OccA$presence=0
points(OccA$decimalLongitude,OccA$decimalLatitude,col=2,pch=2)

Occ=rbind(OccP,OccA)

fwrite(Occ,paste0("./VigieChiro/GIS/PA_",SpTarget,".csv"))
