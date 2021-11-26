Test=T

if(exists("Pipeline")){Test=F}

Coord_HW=function(points,names_coord,bs,bm,layer)
{
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  #OccSL=OccSL[sample.int(nrow(OccSL),10),]
  CoordH=names_coord
  BufferSmall=bs
  BufferMedium=bm
  BufferLarge=bl #too long
  #récupération de la couche habitats et de sa nomenclature
  Layerlist=list.files(layerdir,full.names=T)
  #NomHab=fread("C:/Users/Yves Bas/Downloads/Nom_OSO.csv")
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  
  OccSL$id=c(1:nrow(OccSL))
  
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  
  
  #CRS.new <- CRS(proj4string(CarthageP))
  #OccSL_L93=spTransform(OccSL,CRS("+init=epsg:2154"))
  #OccSL_L93=crop(OccSL_L93,Hab)
  #OccSL=subset(OccSL,OccSL$id %in% OccSL_L93$id)
  Dictionary=tstrsplit(basename(Layerlist),split="_")[[6]]
  NumVar=c(1:length(Layerlist))
  HWdic=data.frame(cbind(NumVar,Dictionary))
  fwrite(HWdic,"HWdic.csv",sep=";")
  
  for (a in 1:length(Layerlist))
  {
    Hab=raster(Layerlist[a])
    
    
    #habitats (OCS)
    Sys.time()
    #DataHp=extract(Hab,OccSL[,],buffer=BufferSmall) #2 sec / buffer
    Sys.time()
    test=extract(Hab,OccSL) #1000 points/sec
    Sys.time()
    OccSL=spCbind(OccSL,test)
    names(OccSL)[ncol(OccSL)]=paste0("SpHW",a,"S")
    
    
    #Hab1=rasterFromCells(Hab,11)
    Sys.time()
    HabufProp=list()
    for (i in 1:nrow(OccSL)) # 2 sec / points
      #for (i in 1:80)
    {
      #radius<-BufferMedium # set the buffer radius
      #buf <- buffer(OccSL[i,], width=radius) # buffer extraction per se
      #Sys.time()
      #Habuf=extract(Hab,buf)
      Sys.time()
      Habuf=extract(Hab,OccSL[i,],buffer=BufferMedium)
      Sys.time()
      
      HabufProp[[i]]=mean(Habuf[[1]],na.rm=T)
      if(i%%100==1){print(paste(i,"/",nrow(OccSL),Sys.time()))}
    }
    
    HabufPropT=unlist(HabufProp)
    HabufPropT[is.na(HabufPropT)]=0
    Sys.time()
    OccSL=spCbind(OccSL,HabufPropT)
    names(OccSL)[ncol(OccSL)]=paste0("SpHW",a,"M")
    
    Sys.time()
    HabufProp=list()
    for (i in 1:nrow(OccSL)) # 2 sec / points
      #for (i in 1:80)
    {
      #radius<-BufferMedium # set the buffer radius
      #buf <- buffer(OccSL[i,], width=radius) # buffer extraction per se
      #Sys.time()
      #Habuf=extract(Hab,buf)
      Sys.time()
      Habuf=extract(Hab,OccSL[i,],buffer=BufferLarge)
      Sys.time()
      
      HabufProp[[i]]=mean(Habuf[[1]],na.rm=T)
      if(i%%100==1){print(paste(i,"/",nrow(OccSL),Sys.time()))}
    }
    
    HabufPropT=unlist(HabufProp)
    HabufPropT[is.na(HabufPropT)]=0
    Sys.time()
    OccSL=spCbind(OccSL,HabufPropT)
    names(OccSL)[ncol(OccSL)]=paste0("SpHW",a,"L")
    
    
    ColSp=subset(names(OccSL),grepl("SpHW",names(OccSL)))
    i=sample(ColSp,1)
    #print(i)
    #print(spplot(OccSL,zcol=i,main=i))
    
    
    
    
  } 
  fwrite(as.data.frame(OccSL),paste0(FOccSL,"_HW.csv"),sep=";")
}

if(Test)
{
  #for test
  Coord_HW(
    points="C:/Users/Yves Bas/Downloads/PrioCoord_DZ_2020-10-02_ZYGOPHYLLACEAE" #table giving coordinates in WGS84
    #points="C:/wamp64/www/sites_localites" #table giving coordinates in WGS84
    ,
    names_coord=c("decimalLongitude","decimalLatitude") #vector of two values giving 
    ,
    bl=50
    ,
    bm=500
    ,
    bl=5000
    ,
    layerdir="./SIG/HabitatsWorld/"
  )
  
}