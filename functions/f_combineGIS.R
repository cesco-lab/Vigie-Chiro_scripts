combineGIS=function(points,names_coord,layerlist)
{
library(data.table)
library(sp)
listGI=layerlist
#listGI=c("ALAN","Alti","Bioclim","Carthage","CLC","OCS")
#Points="RandPts_France_dep_L93Radius_ 91000_1000"
Points=points
#Coord=c("Group.1","Group.2")
Coord=names_coord

tab1=fread(paste0(Points,".csv"))
tab1=unique(tab1,by=Coord)


for (i in 1:length(listGI))
{
  tab2=fread(paste0(Points,"_",listGI[i],".csv"))
  tab2=unique(tab2,by=Coord)
  tab1=merge(tab1,tab2,by=Coord)
}

fwrite(tab1,paste0(dirname(Points),"/GI_",basename(Points),".csv"))
coordinates(tab1)=Coord
SelCol=sample(names(tab1),1)
spplot(tab1,zcol=SelCol,main=SelCol)

}