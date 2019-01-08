library(data.table)
listGI=c("ALAN","Alti","Bioclim","Carthage","CLC","OCS")
Points="RandPts_France_dep_L93Radius_ 28000_1000"
Coord=c("Group.1","Group.2")
#Coord=c("decimalLongitude", "decimalLatitude")


tab1=fread(paste0("./VigieChiro/GIS/",Points,".csv"))
tab1=unique(tab1,by=Coord)

for (i in 1:length(listGI))
{
  tab2=fread(paste0("./VigieChiro/GIS/",Points,"_",listGI[i],".csv"))
  tab2=unique(tab2,by=Coord)
  tab1=merge(tab1,tab2,by=Coord)
}

fwrite(tab1,paste0("./VigieChiro/GIS/GI_",Points,".csv"))
