library(data.table)
library(raster)
library(rgdal)

Sample=2000
SelDep=T
#Dep=c("09","12","31","32","46","65","81","82")
#Dep=c("09","11","12","30","31","32","34","46","48","65","66","81","82")
Dep=c("12","30","34","48")
#Dep=c("30","34")
#Dep=c("75")
#Dep=c("75","77","78","91","92","93","94","95")
Rand=T
SelLongLat=F
LatMin=6297000
LatMax=6308000
LongMin=755000
LongMax=766000




#France_departement
Sys.time()
FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
Sys.time()

Suffix=""
if(SelDep)
{
  FranceD=subset(FranceD,FranceD$DépARTEM0 %in% Dep)
  for (i in 1:length(Dep))
  {
    Suffix=paste(Suffix,Dep[i],sep="_")
  }
  }else{
  Suffix="France"
}

#FranceD=subset(FranceD,select="OBJECT_ID")

if(SelLongLat)
{
  FranceD=crop(FranceD,extent(c(LongMin,LongMax,LatMin,LatMax)))
  Suffix=paste(Suffix,LongMin,LongMax,LatMin,LatMax,sep="_")
  
}

writeOGR(FranceD,dsn="C:/Users/Yves Bas/Documents/VigieChiro/GIS",layer=paste0("FranceD_",Suffix)
         ,driver="ESRI Shapefile",overwrite=T)



if(Rand)
{
  SysGrid=spsample(FranceD,Sample,type="random")
}else{
SysGrid=spsample(FranceD,Sample,type="regular")
}

CRSW84 <- CRS("+init=epsg:4326") # WGS 84
SysG84=spTransform(SysGrid,CRSW84)

CoordSG=as.data.frame(SysG84)
names(CoordSG)=c("Group.1","Group.2")
CoordSG$x=1

plot(SysG84,cex=0.01)

if(Rand)
{
  fwrite(CoordSG,paste0("./VigieChiro/GIS/RandPts_",Suffix,"_",Sample,".csv"))
}else{
fwrite(CoordSG,paste0("./VigieChiro/GIS/SysGrid_",Suffix,"_",Sample,".csv"))
}

