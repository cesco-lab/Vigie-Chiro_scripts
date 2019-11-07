library(data.table)
#library(rgdal)
library(raster)
library(sp)
#library(ggplot2)
#library(MASS)
#library(rgeos)
#pour afficher les milisecondes
op <- options(digits.secs=3)

#recupération des données chiros
refPF=fread("refPF.csv")

#recupération des données chiros
DataCPL3=fread("DataPF_SpNuit2_Seuil90.csv")

#France_departement
FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")


#récupération des données participation
Particip=fread("C:/wamp64/www/p_export.csv")
#récupération des localités
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")

#liste des coordonnées existantes dans ce jeu de données
ListPar=levels(as.factor(DataCPL3$participation))
SelPar=subset(Particip,Particip$participation %in% ListPar)
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
CoordCPL3=aggregate(SelParSL$participation
                    ,by=c(list(SelParSL$longitude),list(SelParSL$latitude),list(SelParSL$participation))
                    ,FUN=length)

Sys.time()
ListSpref=match(DataCPL3$espece,refPF$Espece)
Subref=refPF[ListSpref]
QualifAct=vector()
for (k in 1:nrow(DataCPL3))
{
  if(is.na(Subref$Q25[k]))
  {
    QualifAct=c(QualifAct,NA)
  }else{
    cuts=cbind(-Inf,as.numeric(Subref$Q25[k]),as.numeric(Subref$Q75[k])
               ,as.numeric(Subref$Q98[k]),Inf)
    
    QualifAct=c(QualifAct,findInterval(DataCPL3$nb_contacts[k],cuts,left.open=T))
  }
}
Sys.time()
DataCPL3$QualifAct=QualifAct

DataCoord=merge(DataCPL3,CoordCPL3,by.x="participation",by.y="Group.3")
coordinates(DataCoord) <- c("Group.1", "Group.2")
proj4string(DataCoord) <- CRS("+init=epsg:4326") # WGS 84
FranceWGS84=spTransform(FranceD,CRS(proj4string(DataCoord)))
DataCoord=crop(DataCoord,bbox(FranceWGS84))

DataCoord$espece=as.factor(DataCoord$espece)
breaks <- c(-Inf,1.5,2.5,3.5,Inf)
cols=colorRampPalette(c("yellow", "red"))(length(breaks)-1)

for (i in 1:nlevels(DataCoord$espece))
{
  DataSp=subset(DataCoord,DataCoord$espece==levels(DataCoord$espece)[i])
  if(nrow(DataSp)>1)
  {
  
  png(paste0(levels(DataCoord$espece)[i],"_Act_PF.png"))
  print(spplot(DataSp,zcol="QualifAct",main=paste0(levels(DataCoord$espece)[i],"/ Activite Point Fixe")
         ,col.regions=cols,par.settings =
           list(axis.line = list(col =  'transparent')),sp.layout = FranceWGS84
         ,key=list(lines=TRUE, col="transparent"),cex=1
         ,xlim = bbox(FranceWGS84)[1, ], ylim = bbox(FranceWGS84)[2, ]))
  dev.off()
  
  print(levels(DataCoord$espece)[i])
  }
}





