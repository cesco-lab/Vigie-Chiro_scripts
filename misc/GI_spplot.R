library(data.table)
library(sp)
FGI="GI_Occitanie"

GI=fread(paste0("./VigieChiro/GIS/",FGI,".csv"))

coordinates(GI) <- c("Group.1", "Group.2")
proj4string(GI) <- CRS("+init=epsg:2154") # Lambert 93


for (i in 1:ncol(GI))
{
  if(substr(names(GI)[i],1,2)=="Sp")
  {
    spplot(GI,scol=names(GI)[i],col="transparent",main=names(GI)[i])
  }
}
