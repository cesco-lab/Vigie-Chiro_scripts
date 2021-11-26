library(data.table)
library(raster)
library(sf)
library(lubridate)
library(FNN)

ListPartWavArchiv=fread("C:/wamp64/www/wavarchivees.txt")
ListValid=fread("ValidDataPart.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Zone="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"


FranceD= shapefile(Zone)
p=st_as_sf(FranceD)
pbuf = st_buffer(p, 1000)
FranceD=as(pbuf,'Spatial')

coordinates(SiteLoc)=c("longitude","latitude")
proj4string(SiteLoc)=CRS("+init=epsg:4326")
SiteLoc=spTransform(SiteLoc,proj4string(FranceD))

Tri_FR=over(SiteLoc,FranceD)

SL_FR=subset(SiteLoc,!is.na(Tri_FR$DépARTEM0))

ListVV=subset(ListValid,ListValid$validateur_taxon!="")

ListPWA_NV=subset(ListPartWavArchiv
                  ,!(ListPartWavArchiv$V1 %in% ListVV$participation))

Particip$ProperDate= dmy_hm(Particip$date_debut)
Particip$yday=yday(Particip$ProperDate)

#Point Fixe
PartSL=merge(Particip,SL_FR,by.x=c("site","point"),by.y=c("site","nom"))
PartSL$coord1=scale(PartSL$yday)
hist(PartSL$coord1)
PartSL$coord2=scale(PartSL$longitude)
hist(PartSL$coord2)
PartSL$coord3=scale(PartSL$latitude)
hist(PartSL$coord3)

coordinates(PartSL)=c("coord1","coord2","coord3")

PWA_NV=subset(PartSL,PartSL$participation %in% ListPWA_NV$V1)
PWA_V=subset(PartSL,PartSL$participation %in% ListValid$participation)


g = get.knnx(coordinates(PWA_V), coordinates(PWA_NV),k=1)
PWA_NV$Priority=g[[2]]

fwrite(as.data.table(PWA_NV),"PartNVPriority_PF.csv",sep=";")

#routier-pedestre
Su=unique(as.data.table(SL_FR),by="site")
PartSL=merge(Particip,Su,by.x=c("site"),by.y=c("site"))
PartSL=subset(PartSL,!grepl("Fixe",PartSL$site))
PartSL$coord1=scale(PartSL$yday)
hist(PartSL$coord1)
PartSL$coord2=scale(PartSL$longitude)
hist(PartSL$coord2)
PartSL$coord3=scale(PartSL$latitude)
hist(PartSL$coord3)

coordinates(PartSL)=c("coord1","coord2","coord3")

PWA_NV=subset(PartSL,PartSL$participation %in% ListPWA_NV$V1)
PWA_V=subset(PartSL,PartSL$participation %in% ListValid$participation)


g = get.knnx(coordinates(PWA_V), coordinates(PWA_NV),k=1)
hist(g[[2]])
PWA_NV$Priority=g[[2]]

fwrite(as.data.table(PWA_NV),"PartNVPriority_RP.csv",sep=";")
