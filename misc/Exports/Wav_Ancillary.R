library(data.table)
library(raster)


RepW="C:/Users/yvesb/Documents/chiros/PourFormation2310"
Particip=fread("p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
#Communes=shapefile("C:/Users/yvesb/Documents/SIG/Communes_L93.shp")
CommunesWGS84=shapefile("C:/Users/yvesb/Documents/SIG/Communes_wgs84.shp")


OutputF=paste0(RepW,"/RepQuiz.csv")
WavF=list.files(RepW
                ,recursive=T,full.names=T,pattern=".wav$")


#CommunesWGS84=spTransform(Communes,CRS("+init=epsg:4326"))


SiteW=gsub("Car","Vigiechiro - Point Fixe-",substr(basename(WavF),1,9))
PointW=tstrsplit(basename(WavF),split="-")[[4]]
Particip=Particip[order(Particip$participation,decreasing=T)]
testP=match(paste(SiteW,PointW),paste(Particip$site,Particip$point))
testS=match(paste(SiteW,PointW),paste(SiteLoc$site,SiteLoc$nom))

#testT=match(paste(SiteW,PointW),paste(SiteLoc$site,SiteLoc$nom))

SLsel=SiteLoc[testS,]
table(SLsel$site)

SLsig=SLsel
#SLsig[is.na(SLsig)]=0
SLsig$longitude=ifelse(is.na(SLsig$longitude),mean(SiteLoc$longitude),SLsig$longitude)
SLsig$latitude=ifelse(is.na(SLsig$latitude),mean(SiteLoc$latitude),SLsig$latitude)


#SLsig=subset(SLsig,!is.na(SLsig$longitude))
coordinates(SLsig)=c("longitude","latitude")
proj4string(SLsig)=CRS("+init=epsg:4326")


Auteur=Particip$observateur[testP]

test=raster::intersect(SLsig,CommunesWGS84)


Departement=test$DépARTEM0
table(test$'D\xe9pARTEMEN')
table(Departement)


Commune=test$COMMUNE0




Ancillary=data.frame(Fichier=basename(WavF),Auteur,Departement,Commune)
fwrite(Ancillary,OutputF,sep=";")

