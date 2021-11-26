test=T

if(exists("Pipeline")){test=F}


combineGIS_FR=function(points,names_coord,layerlist)
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
    tab1=subset(tab1,select=subset(names(tab1),(!grepl("Group.1.",names(tab1))&(!grepl("Group.2.",names(tab1))))))
                tab2=fread(paste0(Points,"_",listGI[i],".csv"))
                tab2=unique(tab2,by=Coord)
                tab1=merge(tab1,tab2,by=Coord)
  }
  
  fwrite(tab1,paste0(dirname(Points),"/GI_",basename(Points),".csv"))
  coordinates(tab1)=Coord
  SelCol=sample(names(tab1),1)
  spplot(tab1,zcol=SelCol,main=SelCol)
  
}

if(test)
{
  combineGIS_FR(
    points="coordWGS84_SpNuit2_50_DataLP_PF_exportTot"
    ,
    names_coord=c("Group.1","Group.2")
    ,
    layerlist=c("Bioclim","ALAN","CLCraster","Alti","eoliennes","Carthage","OCS2018bis","Transports")
  )
  
}