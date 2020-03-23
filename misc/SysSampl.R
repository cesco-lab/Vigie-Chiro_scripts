library(data.table)
library(raster)
library(rgdal)
library(sf)
#Zone="C:/Users/Yves Bas/Documents/natura/jasses/domaine.shp"
#Zone="C:/Users/Yves Bas/Documents/VigieChiro/GIS/SMPNR_AdminExpress.shp"
Zone="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
#Zone="C:/Users/Yves Bas/Documents/SIG/Countries/World_L93.shp"
Sample=300000
SelDep=F
Dep=c("France","Spain","Switzerland","Italy")
#Dep=c("France","Italy")
#Dep=c("34")
#Dep=c("2A","2B") #corsica
#Dep=c("09","12","31","32","46","65","81","82") #midipy
#Dep=c("11","30","34","48","66") #lr
#Dep=c("09","11","12","30","31","32","34","46","48","65","66","81","82") #occitanie
#Dep=c("12","30","34","48") #4
#Dep=c("07","11","12","13","30","34","48","81") #8
#Dep=c("07","11","12","13","26","30","34","48","81","84") #10
#Dep=c("04","05","07","09","11","12","13","15"
 #     ,"26","30","31","34","38","42","43","46","48"
  #    ,"63","66","81","82","83","84")
#Dep=c("29","22","35","56","44","49","53","72","85","61","14","50","76","27"
 #     ,"28","37","36","41","45","18","79","86","16","17","75","77","78","91","92"
  #    ,"93","94","95","60","80","02","59","62") #Grand Ouest
#Dep=c("54","55","57","88","67","68","08","10","51","52","90","70","39","25"
 #     ,"58","71","21","89") #Grand Est
#Dep=c("09","11","12","30","31","32","34","46","48","65","66","81","82"
 #     ,"2A","2B","13","83","84","04","05","06","07","26","69","42","38"
  #    ,"73","74","01","03","63","43","15","19","23","87","86","79","17"
   #   ,"16","33","40","47","64","24") #grand sud
#Dep=c("30","34")
#Dep=c("75")
#Dep=c("75","77","78","91","92","93","94","95") #idf
#Dep=c("75","77","78","91","92","93","94","95","76","60","02","51","10","89"
#      ,"45","28","27") #idf + 1 epaisseur

Rand=F
SelLongLat=F
LatMin=4000000
LatMax=8000000
LongMin=-2000000
LongMax=3000000
SelBuffer=F
LatOrigin=6303000 #nddl 
LongOrigin=762000
#LatOrigin=6306000 #toscane
#LongOrigin=1300000
#LatOrigin=6346000 # sisteron
#LongOrigin=934000
#LatOrigin=6546000 # roanne
#LongOrigin=787000
#LatOrigin=6600000 # la motte
#LongOrigin=773000
LatOrigin=6328000 # chezmathieu
LongOrigin=699000
Radius=10000


#France_departement
Sys.time()
FranceD= shapefile(Zone)
Sys.time()

Suffix=""
if(SelDep)
{
if("NAME_ENGL" %in% names(FranceD))
{
  FranceD=subset(FranceD,FranceD$NAME_ENGL %in% Dep)
  
}else{
    FranceD=subset(FranceD,FranceD$DépARTEM0 %in% Dep)
}
    for (i in 1:length(Dep))
  {
    Suffix=paste(Suffix,Dep[i],sep="_")
  }
}

p=st_as_sf(FranceD)
pbuf = st_buffer(p, 1000)
FranceD=as(pbuf,'Spatial')
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

