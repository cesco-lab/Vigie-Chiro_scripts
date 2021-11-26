library(data.table)
library(raster)

WavF=list.files("C:/wamp64/www/Bats/Sel")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Communes=shapefile("./SIG/Communes_L93.shp")
OutputF="C:/wamp64/www/Bats/Sel/RepQuiz210527.csv"

SiteW=gsub("Car","Vigiechiro - Point Fixe-",substr(basename(WavF),1,9))
PointW=tstrsplit(basename(WavF),split="-")[[4]]
Particip=Particip[order(Particip$participation,decreasing=T)]
testP=match(paste(SiteW,PointW),paste(Particip$site,Particip$point))
testS=match(paste(SiteW,PointW),paste(SiteLoc$site,SiteLoc$nom))

SLsig=SiteLoc
coordinates(SLsig)=c("longitude","latitude")
proj4string(SLsig)=CRS("+init=epsg:4326")
CommunesWGS84=spTransform(Communes,CRS("+init=epsg:4326"))
test=intersect(SLsig,CommunesWGS84)

testT=match(paste(SiteW,PointW),paste(SLsig$site,SLsig$nom))

Auteur=Particip$observateur[testP]
Departement=test$DépARTEM0[testT]
Commune=test$COMMUNE0[testT]


Ancillary=data.frame(Fichier=WavF,Auteur,Departement,Commune)
fwrite(Ancillary,OutputF,sep=";")
