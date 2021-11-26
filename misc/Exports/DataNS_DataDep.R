library(data.table)
library(raster)
OccOpen=fread("C:/wamp64/www/DataOpeni.csv")
OccNS=fread("C:/wamp64/www/DataNS.csv")
Zone="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
Thres=0.5

Sys.time()
FranceD= shapefile(Zone)
Sys.time()

CRSW84 <- CRS("+init=epsg:4326") # WGS 84
FranceW84=spTransform(FranceD,CRSW84)

#round some variables
OccOpen$indice_gite=round(OccOpen$indice_gite,3)
OccNS$indice_gite=round(OccNS$indice_gite,3)
OccOpen$indice_reposnocturne=round(OccOpen$indice_reposnocturne,3)
OccNS$indice_reposnocturne=round(OccNS$indice_reposnocturne,3)


#filter bats only for OccOpen
OccOpen=subset(OccOpen,OccOpen$groupe=="Chauve-souris")

#filter out very unprecise taxa for OccNS
StartSp=substr(OccNS$espece,1,1)
Unprecise=(StartSp!=toupper(StartSp))
table(Unprecise)
OccNS=subset(OccNS,!Unprecise)


#filter by departements
coordinates(OccOpen)=c("longitude","latitude")
proj4string(OccOpen)=CRSW84
coordinates(OccNS)=c("longitude","latitude")
proj4string(OccNS)=CRSW84

OccOpen=subset(OccOpen,OccOpen$score_max>Thres)
OccNS=subset(OccNS,OccNS$score_max>Thres)


for (i in 76:length(unique(FranceW84$DépARTEM0)))
{
  print(unique(FranceW84$DépARTEM0)[i])     
  Francei=subset(FranceW84
                 ,FranceW84$DépARTEM0==unique(FranceW84$DépARTEM0)[i])
  Occi=intersect(OccOpen,Francei)
  OccNSi=intersect(OccNS,Francei)
  if(nrow(Occi)>0)
  {
    Occi=subset(Occi,select=names(OccOpen))
    #spplot(Occi,zcol="num_micro")
    fwrite(as.data.frame(Occi)
           ,paste0("C:/wamp64/www/OccDep/DataVigieChiroSensitive_"
                   ,unique(FranceW84$DépARTEM0)[i],".csv"),sep=";")
  }
  if(nrow(OccNSi)>0)
  {
    OccNSi=subset(OccNSi,select=names(OccOpen))
    #spplot(Occi,zcol="num_micro")
    fwrite(as.data.frame(OccNSi)
           ,paste0("C:/wamp64/www/OccDep/DataVigieChiro_"
                   ,unique(FranceW84$DépARTEM0)[i],".csv"),sep=";")
  }
}

#table(OccSpRP$Secteur)
