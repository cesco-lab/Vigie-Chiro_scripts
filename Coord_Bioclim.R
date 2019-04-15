library(data.table)
library(sp)
library(raster)
#OccSL=fread("./vigiechiro/Traits/GBIF/OccSL_bush-cricket.csv")
FOccSL="./vigiechiro/GIS/RandPts_France_dep_L93Radius_ 93000_1000"
#FOccSL="./vigiechiro/GIS/PA_Scorus"
OccSL=fread(paste0(FOccSL,".csv"))
CoordH=c("Group.1", "Group.2")
#CoordH=c("decimalLongitude", "decimalLatitude")

testH=match(CoordH,names(OccSL))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))


#Bioclimdir="C:/Users/Yves Bas/Documents/SIG/Bioclim_5m/" #where bioclim TIFF files are

#coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
coordinates(OccSL) <- CoordH
proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84

r1=getData('worldclim',var='bio',res=0.5,lon=0,lat=45)
r2=getData('worldclim',var='bio',res=0.5,lon=-10,lat=45)


#ListTifiles=list.files(args[2],full.names=T)

  SpBioc1=extract(r1,OccSL)
  SpBioc2=extract(r2,OccSL)
  SpBioc12=mapply(FUN=function(x,y) ifelse(is.na(x),y,x),SpBioc1,SpBioc2)
  SpBioc12=as.data.frame(matrix(SpBioc12,ncol=ncol(SpBioc1),nrow=nrow(SpBioc1)))
subset(c(1:nrow(SpBioc12)),is.na(SpBioc12$V1))
OccSL=subset(OccSL,!is.na(SpBioc12$V1))
SpBioc12=subset(SpBioc12,!is.na(SpBioc12$V1))


names(SpBioc12)=paste0("SpBioC",c(1:ncol(SpBioc12)))

#AFAIRE = récupérer les données côtières qui donnent des NA dans worldclim
Bioclim=cbind(coordinates(OccSL),SpBioc12)  
fwrite(Bioclim,paste0(FOccSL,"_Bioclim.csv"))

coordinates(Bioclim) <- CoordH

SelCol=sample(names(SpBioc12),1)
spplot(Bioclim,zcol=SelCol,main=SelCol)

