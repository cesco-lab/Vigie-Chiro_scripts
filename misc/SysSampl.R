library(data.table)
library(raster)
library(rgdal)
library(sf)
library(rgeos)
#Zone="C:/Users/Yves Bas/Documents/natura/jasses/domaine.shp"
#Zone="C:/Users/Yves Bas/Documents/VigieChiro/GIS/SMPNR_AdminExpress.shp"
Zone="C:/Users/yvesb/Documents/SIG/Limite_administrative/France_dep_L93.shp"
#Zone="C:/Users/Yves Bas/Documents/SIG/Countries/World_L93.shp"
Sample=849906
SelDep=F
Dep=c("France","Spain","Switzerland","Italy","Andorra")
Dep=c("France")
#Dep=c("34","30","13","12","48")
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
SelDepExcl=F
DepExcl=c("Antarctica")

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
#LatOrigin=6328000 # chezmathieu
#LongOrigin=699000
#Radius=c(270000,240000)
Radius=list(c(4410000,4290000)
            ,c(4530000,4410000)
            ,c(4650000,4530000)
            ,c(4770000,4650000)
            ,c(4890000,3770000)
            ,c(5010000,4890000)
            ,c(5130000,5010000)
            ,c(5250000,5130000)
            ,c(5370000,5250000)
            ,c(5490000,5370000)
            ,c(5610000,5490000)
            ,c(5730000,5610000)
            ,c(5850000,5730000)
            ,c(5970000,5850000)
            ,c(6090000,5970000)
            ,c(6210000,6090000)
            ,c(6330000,6210000)
            ,c(6450000,6330000)
            ,c(6570000,6450000)
            ,c(6690000,6570000)
            )
#Radius=90000


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
    FranceD=subset(FranceD,FranceD$D?pARTEM0 %in% Dep)
  }
  for (i in 1:length(Dep))
  {
    Suffix=paste(Suffix,Dep[i],sep="_")
  }
}


if(SelDepExcl)
{
  if("NAME_ENGL" %in% names(FranceD))
  {
    FranceD=subset(FranceD,!FranceD$NAME_ENGL %in% DepExcl)
    
  }else{
    FranceD=subset(FranceD,!FranceD$D?pARTEM0 %in% DepExcl)
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
  if(length(Radius)==1){
    p <- SpatialPoints(cbind(LongOrigin,LatOrigin))
    proj4string(p)=proj4string(FranceD)
    pBuffer=buffer(p,width=Radius)
    FranceD=crop(FranceD,pBuffer)
    Suffix=paste0("Radius_",Radius)
    
  }else{
    if(is.list(Radius))
    {
      FranceList=list()
      SuffixList=list()
      for (a in 1:length(Radius))
      {
        p <- SpatialPoints(cbind(LongOrigin,LatOrigin))
        proj4string(p)=proj4string(FranceD)
        pBuffer=buffer(p,width=Radius[[a]][1])
        pBuffer2=buffer(p,width=Radius[[a]][2])
        #pBuffer2$test=1
        FranceList[[a]]=crop(FranceD,pBuffer)
        FranceList[[a]]=gDifference(FranceList[[a]],pBuffer2)
        SuffixList[[a]]=paste0("Radius_",Radius[[a]][1],"_",Radius[[a]][2])
      }
    }else{
      
      
      p <- SpatialPoints(cbind(LongOrigin,LatOrigin))
      proj4string(p)=proj4string(FranceD)
      pBuffer=buffer(p,width=Radius[1])
      pBuffer2=buffer(p,width=Radius[2])
      #pBuffer2$test=1
      FranceD=crop(FranceD,pBuffer)
      FranceD=gDifference(FranceD,pBuffer2)
      Suffix=paste0("Radius_",Radius[1],"_",Radius[2])
    }
    
  }
}


#if(Suffix!="")
#{
#  writeOGR(FranceD,dsn="C:/Users/Yves Bas/Documents/VigieChiro/GIS"
#          ,layer=paste0(substr(basename(Zone),1,nchar(basename(Zone))-4),Suffix)
#         ,driver="ESRI Shapefile",overwrite=T)
#}

if(!SelBuffer){Radius=""}

for (b in 1:length(Radius))
{
  if(is.list(Radius)){
    FranceD=FranceList[[b]]
    Suffix=SuffixList[[b]]
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
  
  plot(SysG84,cex=0.2)
  
  gc()
  
  if(Rand)
  {
    fwrite(CoordSG,paste0("RandPts_",substr(basename(Zone),1,nchar(basename(Zone))-4)
                          ,Suffix,"_",Sample,".csv"))
  }else{
    fwrite(CoordSG,paste0("SysGrid_",Suffix,"_",Sample,".csv"))
  }
  
}
