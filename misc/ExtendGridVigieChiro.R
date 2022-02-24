library(sp)
library(raster)
library(data.table)
library(stringr)
library(rgdal)

World=shapefile("C:/Users/yvesb/Documents/SIG/Countries/World_L93.shp")
Country="Finland"
DirCoord="C:/Users/yvesb/Documents/SIG/CountryGrids/"
CodeCountries=fread("C:/Users/yvesb/Documents/VigieChiro/GIS/all_IsoCodes.csv",encoding="UTF-8")

testC=match(Country,CodeCountries$name)

if(is.na(testC)){stop("wrong country name, absent from allIsoCodes.csv table")}
#head(World)
CodeCountry=CodeCountries$`alpha-2`[testC]

SubW=subset(World,World$NAME_ENGL==Country)
if(dim(SubW[1])==0){stop("wrong country name, absent from World_L93.shp table")}

Lcoord=list.files(DirCoord,full.names=T,pattern=".csv$")

Ll=list()
for (i in 1:length(Lcoord)){
  Ll[[i]]=fread(Lcoord[i])
  print(names(Ll[[i]]))
  #plot(Ll[[i]]$X,Ll[[i]]$Y,main=Lcoord[i])
  if(names(Ll[[i]])[1]=="AREA"){
  coordinates(Ll[[i]]) <- c("X_COORD", "Y_COORD")
  proj4string(Ll[[i]]) <- CRS("+init=epsg:27572") #Lambert 2 etendu
  Ll[[i]]=as.data.table(Ll[[i]])
  Ll[[i]]$X=Ll[[i]]$X_COORD
  Ll[[i]]$Y=Ll[[i]]$Y_COORD
  Ll[[i]]=subset(Ll[[i]],select=c("X","Y","NUMNAT"))
  }else{
    coordinates(Ll[[i]]) <- c("X", "Y")
    proj4string(Ll[[i]]) <- CRS("+init=epsg:4326") #WGS84
    Ll[[i]]=spTransform(Ll[[i]],CRS("+init=epsg:27572"))
    Ll[[i]]=as.data.table(Ll[[i]])
      }
   

}
Grids=rbindlist(Ll,use.names=T)
Carrenat=Grids
Carrenat$X=round((Carrenat$X-1100)/2000)*2000+1100
Carrenat$Y=round((Carrenat$Y-200)/2000)*2000+200

dim(SubW)
plot(SubW)

writeOGR(SubW,dsn="C:/Users/yvesb/Documents/VigieChiro/GIS"
         ,layer=Country
         ,driver="ESRI Shapefile",overwrite=T)


#transformation in L2E
SubW_L2E=spTransform(SubW,CRS("+init=epsg:27572"))

MinLong=floor((bbox(SubW_L2E)[1,1]-1100)/2000)*2000+1100
MaxLong=ceiling((bbox(SubW_L2E)[1,2]-1100)/2000)*2000+1100
MinLat=floor((bbox(SubW_L2E)[2,1]-200)/2000)*2000+200
MaxLat=ceiling((bbox(SubW_L2E)[2,2]-200)/2000)*2000+200

SerieLong=c(0:as.integer((MaxLong-MinLong)/2000))
SerieLat=c(0:as.integer((MaxLat-MinLat)/2000))
SerieLong=SerieLong*2000+MinLong
SerieLat=SerieLat*2000+MinLat
LongLat=merge(SerieLong,SerieLat)

test=match(paste(LongLat$x,LongLat$y)
                  ,paste(Carrenat$X,Carrenat$Y))
summary(test)
LongLatSel=subset(LongLat,is.na(test))

coordinates(LongLatSel) <- c("x", "y")
proj4string(LongLatSel) <- CRS("+init=epsg:27572") #Lambert 2 etendu

CPL2e=spTransform(SubW,CRS("+init=epsg:27572"))

LongLatSel=intersect(LongLatSel,CPL2e)


NNcar=str_pad(c(1:nrow(LongLatSel)), 6, pad = "0")
NumCar=paste0(CodeCountry,NNcar)
LongLatSel$NumCar=NumCar


LLS_W84=spTransform(LongLatSel,"+init=epsg:4326") #WGS84
head(coordinates(LLS_W84))
LLS_W84=as.data.frame(LLS_W84)
LLS_W84=LLS_W84[,c("x","y","NumCar")]
names(LLS_W84)=c("X","Y","NUMNAT")

fwrite(LLS_W84,paste0(DirCoord,"/carrenat_",CodeCountry,".csv"),sep=";")
