library(sp)
library(raster)
library(data.table)
library(stringr)


Carrenat=shapefile("./SIG/carrenat.shp") # A CHANGER PAR UN CSV QUI COMPILE TOUTES LES GRILLES SINON RISQUE DE DOUBLONS SUR LES PAYS FRONTALIERS ENTRE EUX
CropPol=shapefile("./VigieChiro/GIS/Liechtenstein.shp")
MinLong=1137100   #A CALCULER AUTOMATIQUEMENT
MinLat=2252200
MaxLong=1155100
MaxLat=2278200
CodeCountry="LI"


SerieLong=c(0:as.integer((MaxLong-MinLong)/2000))
SerieLat=c(0:as.integer((MaxLat-MinLat)/2000))
SerieLong=SerieLong*2000+MinLong
SerieLat=SerieLat*2000+MinLat
LongLat=merge(SerieLong,SerieLat)

test=match(paste(LongLat$x,LongLat$y)
                  ,paste(Carrenat$X_COORD,Carrenat$Y_COORD))
summary(test)
LongLatSel=subset(LongLat,is.na(test))

coordinates(LongLatSel) <- c("x", "y")
proj4string(LongLatSel) <- CRS("+init=epsg:27572") #Lambert 2 etendu

CPL2e=spTransform(CropPol,CRS("+init=epsg:27572"))

LongLatSel=intersect(LongLatSel,CPL2e)


NNcar=str_pad(c(1:nrow(LongLatSel)), 6, pad = "0")
NumCar=paste0(CodeCountry,NNcar)
LongLatSel$NumCar=NumCar


LLS_W84=spTransform(LongLatSel,"+init=epsg:4326") #WGS84
head(coordinates(LLS_W84))
LLS_W84=as.data.frame(LLS_W84)
LLS_W84=LLS_W84[,c("x","y","NumCar")]
names(LLS_W84)=c("X","Y","NUMNAT")

fwrite(LLS_W84,paste0("./SIG/carrenat_",CodeCountry,".csv"))
