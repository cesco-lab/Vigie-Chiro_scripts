test=T
#' @param polygon a shapefile path giving the spatial hold to be sampled.
#' @param sample the number of points to be sampled. default is 1000
#' @param dep if specified, character vector giving the list of departement to be selected (polygon should be kept by default)
#' @param rand boolean, allowing a random sampling instead of systematic by default
#' @param latmin if specified, minimum latitude to select
#' @param latmax if specified, maximum latitude to select
#' @param longmin if specified, minimum longitude to select
#' @param longmax if specified, maximum longitude to select
#' @param buffer if specified, buffer size of the selection (latorigin and longorigin must be specified)
#' @param latorigin if specified, buffer size of the selection (buffer and longorigin must be specified)
#' @param longorigin if specified, buffer size of the selection (latorigin and buffer must be specified)

SysSampl=function(polygon,sample=1000,dep=NA,rand=F,latmin=NA,latmax=NA
                  ,longmin=NA,longmax=NA,buffer=NA,latorigin=NA,longorigin=NA
                  ,output)
{
  library(data.table)
  library(raster)
  library(rgdal)
  Zone=polygon
  Sample=sample
  Dep=dep
  Rand=rand
  LatMin=latmin
  LatMax=latmax
  LongMin=longmin
  LongMax=longmax
  LatOrigin=latorigin
  LongOrigin=longorigin
  Radius=buffer
  
  
  #France_departement
  Sys.time()
  FranceD= shapefile(Zone)
  
  Sys.time()
  
  Suffix=""
  if(!is.na(Dep)[1])
  {
    FranceD=subset(FranceD,FranceD$DépARTEM0 %in% Dep)
    for (i in 1:length(Dep))
    {
      Suffix=paste(Suffix,Dep[i],sep="_")
    }
  }
  
  #FranceD=subset(FranceD,select="OBJECT_ID")
  
  if(!is.na(LatMin))
  {
    FranceD=crop(FranceD,extent(c(LongMin,LongMax,LatMin,LatMax)))
    Suffix=paste(Suffix,LongMin,LongMax,LatMin,LatMax,sep="_")
    
  }
  
  if(!is.na(Radius))
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
    fwrite(CoordSG,paste0(output,"/RandPts_",substr(basename(Zone),1,nchar(basename(Zone))-4)
                          ,Suffix,"_",Sample,".csv"))
  }else{
    fwrite(CoordSG,paste0(output,"/SysGrid_",Suffix,"_",Sample,".csv"))
  }
}

if(test)
{
  SysSampl(
    polygon="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
    #Dep=c("34")
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
    ,dep=c("09","11","12","30","31","32","34","46","48","65","66","81","82"
           ,"2A","2B","13","83","84","04","05","06","07","26","69","42","38"
           ,"73","74","01","03","63","43","15","19","23","87","86","79","17"
           ,"16","33","40","47","64","24") #grand sud
    #Dep=c("30","34")
    #Dep=c("75")
    #Dep=c("75","77","78","91","92","93","94","95") #idf
    ,rand=T
    ,output="./VigieChiro/GIS/"  
  )
}