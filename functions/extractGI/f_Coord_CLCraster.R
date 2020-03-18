Test=F
Coord_CLCraster=function(points,names_coord,bm,bl,layer)
{
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  #OccSL=OccSL[sample.int(nrow(OccSL),10),]
  CoordH=names_coord
  BufferMedium=bm
  BufferLarge=bl
  #récupération de la couche habitats et de sa nomenclature
  Hab=raster(layer)
  #NomHab=fread("C:/Users/Yves Bas/Downloads/Nom_OSO.csv")
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  
  OccSL$id=c(1:nrow(OccSL))
  
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  #CRS.new <- CRS(proj4string(CarthageP))
  OccSL_L93=spTransform(OccSL,proj4string(Hab))
  OccSL_L93=crop(OccSL_L93,Hab)
  OccSL=subset(OccSL,OccSL$id %in% OccSL_L93$id)
  
  
  
  #extract CLC habitats for medium buffers
  Sys.time()
  HabufProp=list()
  for (i in 1:nrow(OccSL)) # 2 sec / points
    #for (i in 1:80)
  {
    radius<-BufferMedium # set the buffer radius
    buf <- buffer(OccSL[i,], width=radius) # buffer extraction per se
    Sys.time()
    Habuf=extract(Hab,buf)
    Sys.time()
    HabufProp[[i]]=as.data.table(t(as.matrix(table(Habuf))/sum(table(Habuf))))
    if(nrow(HabufProp[[i]])>0) {HabufProp[[i]]$id=OccSL$id[i]}
    if(i%%100==1){print(paste(i,"/",nrow(OccSL),Sys.time()))}
  }
  
  HabufPropT=rbindlist(HabufProp,fill=T)
  HabufPropT[is.na(HabufPropT)]=0
  Sys.time()
  
  Id=HabufPropT$id
  HabufPropT$id=NULL
  colnames(HabufPropT)=paste0("SpHC",colnames(HabufPropT),"M")
  
  #select three-levels habitats
  testNC=(nchar(colnames(HabufPropT))==8)
  N3=colnames(HabufPropT)[testNC]
  HabufPropT3l=HabufPropT[,..N3]
  Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,6)))
  
  if(nrow(Prop32)>0)
  {
    #sum three-levels habitats 
    for (i in 1:nrow(Prop32))
    {
      testi=substr(colnames(HabufPropT3l),1,6)==Prop32$V1[i]
      Ni=colnames(HabufPropT3l)[testi]
      HabufPropTi=HabufPropT3l[,..Ni]
      HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
      HabufPropT=cbind(HabufPropT,HabufPropTsum)
      names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"M")
    }
  }
  
  
  #select 2-levels habitats
  testNC=(nchar(colnames(HabufPropT))==7)
  N3=colnames(HabufPropT)[testNC]
  HabufPropT3l=HabufPropT[,..N3]
  Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,5)))
  
  if(nrow(Prop32)>0)
  {
    #sum 2-levels habitats 
    for (i in 1:nrow(Prop32))
    {
      testi=substr(colnames(HabufPropT3l),1,5)==Prop32$V1[i]
      Ni=colnames(HabufPropT3l)[testi]
      HabufPropTi=HabufPropT3l[,..Ni]
      HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
      HabufPropT=cbind(HabufPropT,HabufPropTsum)
      names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"M")
    }
  }
  
  HabufPropT$id=Id
  OccSL3=merge(OccSL,HabufPropT,by="id")
  
  i=sample(c(1:ncol(HabufPropT)),1)
  print(names(HabufPropT)[i])
  print(spplot(OccSL3,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
  
  
  Sys.time()
  HabufProp=list()
  for (i in 1:nrow(OccSL)) # 2 sec / points
    #for (i in 1:80)
  {
    radius<-BufferLarge # set the buffer radius
    buf <- buffer(OccSL[i,], width=radius) # buffer extraction per se
    Sys.time()
    Habuf=extract(Hab,buf)
    Sys.time()
    HabufProp[[i]]=as.data.table(t(as.matrix(table(Habuf))/sum(table(Habuf))))
    if(nrow(HabufProp[[i]])>0) {HabufProp[[i]]$id=OccSL$id[i]}
    if(i%%100==1){print(paste(i,"/",nrow(OccSL),Sys.time()))}
  }
  
  HabufPropT=rbindlist(HabufProp,fill=T)
  HabufPropT[is.na(HabufPropT)]=0
  Sys.time()
  
  Id=HabufPropT$id
  HabufPropT$id=NULL
  colnames(HabufPropT)=paste0("SpHC",colnames(HabufPropT),"L")
  
  #select three-levels habitats
  testNC=(nchar(colnames(HabufPropT))==8)
  N3=colnames(HabufPropT)[testNC]
  HabufPropT3l=HabufPropT[,..N3]
  Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,6)))
  
  if(nrow(Prop32)>0)
  {
    #sum three-levels habitats 
    for (i in 1:nrow(Prop32))
    {
      testi=substr(colnames(HabufPropT3l),1,6)==Prop32$V1[i]
      Ni=colnames(HabufPropT3l)[testi]
      HabufPropTi=HabufPropT3l[,..Ni]
      HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
      HabufPropT=cbind(HabufPropT,HabufPropTsum)
      names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"L")
    }
  }
  
  
  #select 2-levels habitats
  testNC=(nchar(colnames(HabufPropT))==7)
  N3=colnames(HabufPropT)[testNC]
  HabufPropT3l=HabufPropT[,..N3]
  Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,5)))
  
  if(nrow(Prop32)>0)
  {
    #sum 2-levels habitats 
    for (i in 1:nrow(Prop32))
    {
      testi=substr(colnames(HabufPropT3l),1,5)==Prop32$V1[i]
      Ni=colnames(HabufPropT3l)[testi]
      HabufPropTi=HabufPropT3l[,..Ni]
      HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
      HabufPropT=cbind(HabufPropT,HabufPropTsum)
      names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"L")
    }
  }
  
  HabufPropT$id=Id
  OccSL4=merge(OccSL3,HabufPropT,by="id")
  
  i=sample(c(1:ncol(HabufPropT)),1)
  print(names(HabufPropT)[i])
  print(spplot(OccSL4,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
  
  
  
  
  
  OccSL_ARajouter=subset(OccSL4,select=grepl("Sp",names(OccSL3)))
  
  OCS=data.frame(cbind(coordinates(OccSL4),as.data.frame(OccSL_ARajouter)))
  fwrite(OCS,paste0(FOccSL,"_CLCraster.csv"))
  
  coordinates(OCS) <- CoordH
  
  SelCol=sample(names(OccSL_ARajouter),1)
  spplot(OCS,zcol=SelCol,main=SelCol)
  
}


if(Test)
{
  #for test
  Coord_CLCraster(
    points="PrioCoord_2020-02-20" #table giving coordinates in WGS84
    ,
    names_coord=c("decimalLongitude","decimalLatitude") #vector of two values giving 
    ,
    bm=500
    ,
    bl=5000
    ,
    layer="./SIG/clc2018_clc2018_v2018_20_raster100m/CLC2018_CLC2018_V2018_20.tif"
  )
  
}
