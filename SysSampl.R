library(data.table)
library(raster)
library(rgdal)
#Zone="C:/Users/Yves Bas/Documents/natura/jasses/domaine.shp"
Zone="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
Sample=10000
SelDep=T
#Dep=c("09","12","31","32","46","65","81","82") #midipy
Dep=c("09","11","12","30","31","32","34","46","48","65","66","81","82") #occitanie
#Dep=c("12","30","34","48") #4
#Dep=c("07","11","12","13","30","34","48","81") #8
#Dep=c("07","11","12","13","26","30","34","48","81","84") #10
#Dep=c("04","05","07","09","11","12","13","15"
 #     ,"26","30","31","34","38","42","43","46","48"
  #    ,"63","66","81","82","83","84")

#Dep=c("30","34")
#Dep=c("75")
#Dep=c("75","77","78","91","92","93","94","95") #idf
Rand=F
SelLongLat=F
LatMin=6297000
LatMax=6308000
LongMin=755000
LongMax=766000
SelBuffer=F
LatOrigin=6303000
LongOrigin=762000
Radius=91000


#France_departement
Sys.time()
FranceD= shapefile(Zone)

Sys.time()

Suffix=""
if(SelDep)
{
  FranceD=subset(FranceD,FranceD$DépARTEM0 %in% Dep)
  for (i in 1:length(Dep))
  {
    Suffix=paste(Suffix,Dep[i],sep="_")
  }
}

#FranceD=subset(FranceD,select="OBJECT_ID")

if(SelLongLat)
{
  FranceD=crop(FranceD,extent(c(LongMin,LongMax,LatMin,LatMax)))
  Suffix=paste(Suffix,LongMin,LongMax,LatMin,LatMax,sep="_")
  
}

if(SelBuffer)
{
  p <- SpatialPoints(cbind(LongOrigin,LatOrigin))
  proj4string(p)=proj4string(FranceD)
  pBuffer=buffer(p,width=Radius)
  FranceD=crop(FranceD,pBuffer)
  Suffix=paste("Radius_",Radius)
}



if(Suffix!="")
  {
writeOGR(FranceD,dsn="C:/Users/Yves Bas/Documents/VigieChiro/GIS"
         ,layer=paste0(substr(basename(Zone),1,nchar(basename(Zone))-4),Suffix)
         ,driver="ESRI Shapefile",overwrite=T)
}


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
CoordSG$id=c(1:nrow(CoordSG))

plot(SysG84,cex=1)

if(Rand)
{
  fwrite(CoordSG,paste0("./VigieChiro/GIS/RandPts_",substr(basename(Zone),1,nchar(basename(Zone))-4)
                        ,Suffix,"_",Sample,".csv"))
}else{
fwrite(CoordSG,paste0("./VigieChiro/GIS/SysGrid_",Suffix,"_",Sample,".csv"))
}

