Test=F
Coord_BioclimLocal=function(points,names_coord,bm,bl,layer)
{
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  CoordH=names_coord 
  asc_files<- list.files(layer
                         ,pattern =".tif$",full.names=T)
  
  GrossBioclim=shapefile(layCorr)
  
  #GrossBioclim=spTransform(GrossBioclim,CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  
  for(i in 1:length(asc_files)) {
    rasti <- raster(asc_files[i]) 
    Sys.time()
    SpBioci=extract(rasti,OccSL) # 0.0004 sec / points
    print(paste(i,Sys.time()))
    OccSL=spCbind(OccSL,SpBioci)
    NumBioci=tstrsplit(basename(asc_files[i]),split="_")[[4]]
    NumBioci=gsub(".tif","",NumBioci)
    print(spplot(OccSL,zcol="SpBioci",main=NumBioci))
    names(OccSL)[ncol(OccSL)]=paste0("SpBioC",NumBioci)
    }
  
  #multiplier par 10 : 1-2 4-11
  #for back-compatibility with old-fashioned Bioclim in 1e-1°C
  OccSL$SpBioC1=OccSL$SpBioC1*10
  OccSL$SpBioC2=OccSL$SpBioC2*10
  OccSL$SpBioC4=OccSL$SpBioC4*10
  OccSL$SpBioC5=OccSL$SpBioC5*10
  OccSL$SpBioC6=OccSL$SpBioC6*10
  OccSL$SpBioC7=OccSL$SpBioC7*10
  OccSL$SpBioC8=OccSL$SpBioC8*10
  OccSL$SpBioC9=OccSL$SpBioC9*10
  OccSL$SpBioC10=OccSL$SpBioC10*10
  OccSL$SpBioC11=OccSL$SpBioC11*10
 
  OccSL_NA=subset(OccSL,is.na(OccSL$SpBioC1))
  OccSL_A=subset(OccSL,!is.na(OccSL$SpBioC1))
  Sys.time()
  if(nrow(OccSL_NA)>0)
  {
    GrossBioclim=spTransform(GrossBioclim,CRS(proj4string(OccSL_NA)))
    
  OccSL_Add=over(OccSL_NA,GrossBioclim)
  Sys.time()
  names(OccSL_Add)=gsub("bio","SpBioC",names(OccSL_Add))
  OccSL_NAimp=subset(OccSL_NA
                     ,select=subset(names(OccSL_NA)
                      ,!grepl("SpBioC",names(OccSL_NA))))
                                                                   
  OccSL_NAdd=spCbind(OccSL_NAimp,OccSL_Add[,2:20])
  #print(spplot(OccSL_NAdd,zcol="SpBioC1"))
  #print(spplot(OccSL_A,zcol="SpBioC1"))
  OccSL_NAdd=as.data.frame(OccSL_NAdd)
  OccSL_A=as.data.frame(OccSL_A)
  
  OccSL_All=rbind(OccSL_A,OccSL_NAdd)
  
  }else{
    OccSL_All=as.data.frame(OccSL_A)
  }
  
  #OccSL_Allsp=OccSL_All
  #coordinates(OccSL_Allsp)=names_coord
  #print(spplot(OccSL_Allsp,zcol="SpBioC3"))
  
  
  fwrite(OccSL_All,paste0(FOccSL,"_Bioclim.csv"))
  
  
}

if(Test)
{
#for test
Coord_BioclimLocal(
  points="PrioCoord_DE_2020-09-29_GROSSULARIACEAE" #table giving coordinates in WGS84
  ,
  names_coord=c("decimalLongitude","decimalLatitude") #vector of two values giving 
  ,
  layer="D:/Bioclim30s"
  ,
  layCorr="GrossV.shp"
)

}