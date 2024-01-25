library(data.table)
library(raster)

DirQuiz="C:/Users/yvesb/Documents/www/Sample220329"
#Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
CommunesWGS84=shapefile("C:/Users/yvesb/Documents/CommunesSHPwgs84.shp")

#CommunesWGS84=spTransform(Communes,CRS("+init=epsg:4326"))

ListW=list.files(DirQuiz,recursive=T,full.names=T)
BW=basename(ListW)
Carre=gsub("Car","Vigiechiro - Point Fixe-",tstrsplit(BW,split="-")[[1]])
Point=tstrsplit(BW,split="-")[[4]]
testCP=match(paste(Carre,Point),paste(SiteLoc$site,SiteLoc$nom))
SiteLocSel=SiteLoc[testCP,]
SiteLocSel$observateur
Lieu=vector()
for (i in 1:nrow(SiteLocSel)){
  SiteLoci=SiteLocSel[i,]
  if(!is.na(SiteLoci$latitude[1])){
  coordinates(SiteLoci)=c("longitude","latitude")
  proj4string(SiteLoci)=CRS("+init=epsg:4326")
  Ci=intersect(SiteLoci,CommunesWGS84)
  #print(dim(Ci))
  Lieu=c(Lieu,paste0(Ci$COMMUNE0[1],", ",Ci$DÃ.pARTEM0[1]))
  }else{
    Lieu=c(Lieu,"")
    
  }
}
Table=data.frame(Repertoire=basename(dirname(ListW)),Fichier=BW,Lieu,Auteur=SiteLocSel$observateur)

fwrite(Table,paste0(DirQuiz,"/0_Infos.csv"),sep=";")

