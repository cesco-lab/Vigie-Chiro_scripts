library(data.table)
library(sp)
library(raster)
library(maptools)
#library(Rnightlights)
#OccSL=fread("./vigiechiro/Traits/GBIF/OccSL_bush-cricket.csv")
FOccSL="./vigiechiro/GIS/RandPts_France_dep_L93Radius_ 93000_1000"
#FOccSL="./vigiechiro/GIS/PA_Scorus"
OccSL=fread(paste0(FOccSL,".csv"))
CoordH=c("Group.1", "Group.2")
#CoordH=c("decimalLongitude", "decimalLatitude")
BufferLarge=5000


testH=match(CoordH,names(OccSL))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))

#ALAN=getCtryNlData("FRA","FRA_adm0",nlTypes="OLS.Y",nlPeriods=2012,nlStats="mean")
ALAN=raster("C:/Users/Yves Bas/Downloads/SVDNB_npp_20161201-20161231_75N060W_vcmslcfg_v10_c201701271138.avg_rade9.tif")

coordinates(OccSL) <- CoordH
proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84

Sys.time()
SpALAN_M=extract(ALAN,OccSL)
Sys.time()
OccSL=spCbind(OccSL,SpALAN_M)
#OccSL$test=log(OccSL$SpALAN_M)
spplot(OccSL,zcol="SpALAN_M",main="SpALAN_M",col="transparent")
#spplot(OccSL,zcol="test",main="SpALAN_M",col="transparent")

Sys.time()
SpALAN_L=extract(ALAN,OccSL,buffer=BufferLarge,fun=mean) # 0.1 sec / données
Sys.time()
OccSL=spCbind(OccSL,SpALAN_L)

spplot(OccSL,zcol="SpALAN_L",main="SpALAN_L",col="transparent")

ALAN=data.frame(cbind(coordinates(OccSL),SpALAN_M,SpALAN_L))

fwrite(ALAN,paste0(FOccSL,"_ALAN.csv"))

coordinates(ALAN) <- CoordH

SelCol=sample(c("SpALAN_M","SpALAN_L"),1)
spplot(ALAN,zcol=SelCol,main=SelCol)

