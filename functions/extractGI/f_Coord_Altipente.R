#Consideration of altitude and slope

##########INPUTS################
# points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)

Test=T

if(exists("Pipeline")){Test=F}


Coord_Alti=function(points,names_coord,bm,bl,layer)
{
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  CoordH=names_coord 
  BufferMedium=bm
  BufferLarge=bl
  asc_files<- list.files(layer
                         ,pattern =".asc$",full.names=T)
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  #CRS.new <- CRS(proj4string(CarthageP))
  OccSL_L93=spTransform(OccSL,CRS("+init=epsg:2154"))
  
  #extraction des donn�es alti
  rast.list <- list()
  for(i in 1:length(asc_files)) { rast.list[i] <- raster(asc_files[i]) }
  rast.list$fun <- mean
  Sys.time()
  AltiTot <- do.call(mosaic,rast.list) # 8 min
  Sys.time()
  #plot(AltiTot)
  Sys.time()
  
  ####################################################
  ####################################################
  #############Extraction altitude####################
  ####################################################
  ####################################################
  #extraction des altitudes ponctuelles
  
  
  #######
  #Buffer S
  #######
  Sys.time()
  SpAltiS=extract(AltiTot,OccSL_L93) # < 0.001 sec / points
  SpAltiS[is.na(SpAltiS)]=0
  OccSL=spCbind(OccSL,SpAltiS)
  spplot(OccSL,zcol="SpAltiS",main="SpAltiS")
  
  #######
  #Buffer M
  #######
  Sys.time()
  SpAltiM=extract(AltiTot,OccSL_L93,buffer=BufferMedium,fun=mean) # 0.01 sec / points
  SpAltiM[is.na(SpAltiM)]=0
  OccSL=spCbind(OccSL,SpAltiM)
  spplot(OccSL,zcol="SpAltiM",main="SpAltiM")
  
  #######
  #Buffer L
  #######
  Sys.time()
  SpAltiL=extract(AltiTot,OccSL_L93,buffer=BufferLarge,fun=mean) # 0.02 sec / points
  SpAltiM[is.na(SpAltiM)]=0
  
  Sys.time()
  
  OccSL=spCbind(OccSL,SpAltiL)
  spplot(OccSL,zcol="SpAltiL",main="SpAltiL")
  
  ####################################################
  ####################################################
  #############Calcul de la SpPen#####################
  ####################################################
  ####################################################
  
  #ajout de 8 points cardinaux � 75m de chaques points (N,S,E,O,NO,NE,SE,SO)
  
  
  Coord=as.data.frame(OccSL_L93) #extraire les colonnes x, Group1 et Group2
  Coord$Group.1=as.data.frame(subset(Coord
                                     ,select=names_coord[1]))[,1]
  Coord$Group.2=as.data.frame(subset(Coord
                                     ,select=names_coord[2]))[,1]
  
  
  ListePointCard=data.frame()
  for (k in 1:nrow(Coord)){
    
    x=c(0,0,0,0,0,0,0,0)
    Group.1=c(Coord$Group.1[k]+75,Coord$Group.1[k]-75,Coord$Group.1[k]+75,Coord$Group.1[k],Coord$Group.1[k]-75,Coord$Group.1[k],Coord$Group.1[k]-75,Coord$Group.1[k]+75)
    Group.2=c(Coord$Group.2[k]+75,Coord$Group.2[k]-75,Coord$Group.2[k],Coord$Group.2[k]+75,Coord$Group.2[k],Coord$Group.2[k]-75,Coord$Group.2[k]+75,Coord$Group.2[k]-75)
    
    PointsCard=data.frame(x,Group.1,Group.2)
    ListePointCard=rbind(ListePointCard,PointsCard)
  }
  
  coordinates(ListePointCard) <- c("Group.1","Group.2")
  proj4string(ListePointCard) <- CRS("+init=epsg:2154")
  
  #######
  #Buffer S
  #######
  AltiListePointCard=extract(AltiTot,ListePointCard) #liste altitudes des points Cardinaux
  AltiListePointCard[is.na(AltiListePointCard)]=0
  
  #calcul de la SpPen maximale (en degr�)
  SpPenS=vector()
  SpNorS=vector()
  SpEasS=vector()
  SpSumS=vector()
  for(k in 1:(nrow(Coord))){
    SpPenk=max(atan(abs(AltiListePointCard[(k-1)*8+1]-SpAltiS[k])/75),atan(abs(AltiListePointCard[(k-1)*8+2]-SpAltiS[k])/75),atan(abs(AltiListePointCard[(k-1)*8+3]-SpAltiS[k])/75),atan(abs(AltiListePointCard[(k-1)*8+4]-SpAltiS[k])/75),atan(abs(AltiListePointCard[(k-1)*8+5]-SpAltiS[k])/75),atan(abs(AltiListePointCard[(k-1)*8+6]-SpAltiS[k])/75),atan(abs(AltiListePointCard[(k-1)*8+7]-SpAltiS[k])/75),atan(abs(AltiListePointCard[(k-1)*8+8]-SpAltiS[k])/75))
    SpNork=atan((AltiListePointCard[(k-1)*8+4]-AltiListePointCard[(k-1)*8+6])/75*2)
    SpEask=atan((AltiListePointCard[(k-1)*8+3]-AltiListePointCard[(k-1)*8+5])/75*2)
    SpSumk=SpAltiS[k]-mean(AltiListePointCard[((k-1)*8+1):((k-1)*8+8)])
    SpPenS=c(SpPenS,SpPenk)
    SpNorS=c(SpNorS,SpNork)
    SpEasS=c(SpEasS,SpEask)
    SpSumS=c(SpSumS,SpSumk)
    
    }
  OccSL=spCbind(OccSL,SpPenS)
  OccSL=spCbind(OccSL,SpNorS)
  OccSL=spCbind(OccSL,SpEasS)
  OccSL=spCbind(OccSL,SpSumS)
  
  #######
  #Buffer M
  #######
  ListePointCard=data.frame()
  for (k in 1:nrow(Coord)){
    
    x=c(0,0,0,0,0,0,0,0)
    Group.1=c(Coord$Group.1[k]+BufferMedium,Coord$Group.1[k]-BufferMedium,Coord$Group.1[k]+BufferMedium,Coord$Group.1[k],Coord$Group.1[k]-BufferMedium,Coord$Group.1[k],Coord$Group.1[k]-BufferMedium,Coord$Group.1[k]+BufferMedium)
    Group.2=c(Coord$Group.2[k]+BufferMedium,Coord$Group.2[k]-BufferMedium,Coord$Group.2[k],Coord$Group.2[k]+BufferMedium,Coord$Group.2[k],Coord$Group.2[k]-BufferMedium,Coord$Group.2[k]+BufferMedium,Coord$Group.2[k]-BufferMedium)
    
    PointsCard=data.frame(x,Group.1,Group.2)
    ListePointCard=rbind(ListePointCard,PointsCard)
  }
  coordinates(ListePointCard) <- c("Group.1","Group.2")
  proj4string(ListePointCard) <- CRS("+init=epsg:2154")
  
  
  AltiListePointCard=extract(AltiTot,ListePointCard)
  
  for (z in 1:length(AltiListePointCard))
  {
  if(is.na(AltiListePointCard[z]))
  {
    AltiListePointCard[z]=SpAltiS[floor(z/8)+1]
  }
  }
  AltiListePointCard[is.na(AltiListePointCard)]=0
  
  #calcul de la SpPen maximale (en degr�)
  SpPenM=vector()
  SpNorM=vector()
  SpEasM=vector()
  SpSumM=vector()
  
  for(k in 1:(nrow(Coord))){
    SpPenk=max(atan(abs(AltiListePointCard[(k-1)*8+1]-SpAltiS[k])/BufferMedium),atan(abs(AltiListePointCard[(k-1)*8+2]-SpAltiS[k])/BufferMedium),atan(abs(AltiListePointCard[(k-1)*8+3]-SpAltiS[k])/BufferMedium),atan(abs(AltiListePointCard[(k-1)*8+4]-SpAltiS[k])/BufferMedium),atan(abs(AltiListePointCard[(k-1)*8+5]-SpAltiS[k])/BufferMedium),atan(abs(AltiListePointCard[(k-1)*8+6]-SpAltiS[k])/BufferMedium),atan(abs(AltiListePointCard[(k-1)*8+7]-SpAltiS[k])/BufferMedium),atan(abs(AltiListePointCard[(k-1)*8+8]-SpAltiS[k])/BufferMedium))
    SpNork=atan((AltiListePointCard[(k-1)*8+4]-AltiListePointCard[(k-1)*8+6])/BufferMedium*2)
    SpEask=atan((AltiListePointCard[(k-1)*8+3]-AltiListePointCard[(k-1)*8+5])/BufferMedium*2)
    SpSumk=SpAltiS[k]-mean(AltiListePointCard[((k-1)*8+1):((k-1)*8+8)])
    SpPenM=c(SpPenM,SpPenk)
    SpNorM=c(SpNorM,SpNork)
    SpEasM=c(SpEasM,SpEask)
    SpSumM=c(SpSumM,SpSumk)
    
  }
  OccSL=spCbind(OccSL,SpPenM)
  OccSL=spCbind(OccSL,SpNorM)
  OccSL=spCbind(OccSL,SpEasM)
  OccSL=spCbind(OccSL,SpSumM)
  
  #######
  #Buffer L
  #######
  ListePointCard=data.frame()
  for (k in 1:nrow(Coord)){
    
    x=c(0,0,0,0,0,0,0,0)
    Group.1=c(Coord$Group.1[k]+BufferLarge,Coord$Group.1[k]-BufferLarge,Coord$Group.1[k]+BufferLarge,Coord$Group.1[k],Coord$Group.1[k]-BufferLarge,Coord$Group.1[k],Coord$Group.1[k]-BufferLarge,Coord$Group.1[k]+BufferLarge)
    Group.2=c(Coord$Group.2[k]+BufferLarge,Coord$Group.2[k]-BufferLarge,Coord$Group.2[k],Coord$Group.2[k]+BufferLarge,Coord$Group.2[k],Coord$Group.2[k]-BufferLarge,Coord$Group.2[k]+BufferLarge,Coord$Group.2[k]-BufferLarge)
    
    PointsCard=data.frame(x,Group.1,Group.2)
    ListePointCard=rbind(ListePointCard,PointsCard)
  }
  coordinates(ListePointCard) <- c("Group.1","Group.2")
  proj4string(ListePointCard) <- CRS("+init=epsg:2154")
  
  
  AltiListePointCard=extract(AltiTot,ListePointCard)
  
  for (z in 1:length(AltiListePointCard))
  {
    if(is.na(AltiListePointCard[z]))
    {
      AltiListePointCard[z]=SpAltiS[floor(z/8)+1]
    }
  }
  AltiListePointCard[is.na(AltiListePointCard)]=0
  
  
  #calcul de la SpPen maximale (en degr�)
  SpPenL=vector()
  SpNorL=vector()
  SpEasL=vector()
  SpSumL=vector()
  for(k in 1:(nrow(Coord))){
    SpPenk=max(atan(abs(AltiListePointCard[(k-1)*8+1]-SpAltiS[k])/BufferLarge),atan(abs(AltiListePointCard[(k-1)*8+2]-SpAltiS[k])/BufferLarge),atan(abs(AltiListePointCard[(k-1)*8+3]-SpAltiS[k])/BufferLarge),atan(abs(AltiListePointCard[(k-1)*8+4]-SpAltiS[k])/BufferLarge),atan(abs(AltiListePointCard[(k-1)*8+5]-SpAltiS[k])/BufferLarge),atan(abs(AltiListePointCard[(k-1)*8+6]-SpAltiS[k])/BufferLarge),atan(abs(AltiListePointCard[(k-1)*8+7]-SpAltiS[k])/BufferLarge),atan(abs(AltiListePointCard[(k-1)*8+8]-SpAltiS[k])/BufferLarge))
    SpNork=atan((AltiListePointCard[(k-1)*8+4]-AltiListePointCard[(k-1)*8+6])/BufferLarge*2)
    SpEask=atan((AltiListePointCard[(k-1)*8+3]-AltiListePointCard[(k-1)*8+5])/BufferLarge*2)
    SpSumk=SpAltiS[k]-mean(AltiListePointCard[((k-1)*8+1):((k-1)*8+8)])
    SpPenL=c(SpPenL,SpPenk)
    SpNorL=c(SpNorL,SpNork)
    SpEasL=c(SpEasL,SpEask)
    SpSumL=c(SpSumL,SpSumk)
    
  }
  OccSL=spCbind(OccSL,SpPenL)
  OccSL=spCbind(OccSL,SpNorL)
  OccSL=spCbind(OccSL,SpEasL)
  OccSL=spCbind(OccSL,SpSumL)
  
  
  ##################SUITE#######################
  
  
  Alti=data.frame(cbind(coordinates(OccSL),SpAltiS,SpAltiM,SpAltiL
                        ,SpPenS,SpPenM,SpPenL,SpNorS,SpNorM,SpNorL
                        ,SpEasS,SpEasM,SpEasL,SpSumS,SpSumM,SpSumL))
  
  #test=subset(Alti,(Alti$SpPenL>1.24)&(Alti$SpAltiS<50))
  
  
  fwrite(Alti,paste0(FOccSL,"_Alti.csv"))
  
  coordinates(Alti) <- CoordH
  SpCol=subset(names(Alti),grepl("Sp",names(Alti)))
  SelCol=sample(SpCol,1)
  spplot(Alti,zcol=SelCol,main=SelCol)
  
}

if(Test)
{
  #for test
  Coord_Alti(
    #points="./VigieChiro/GIS/SysGrid__1000" #table giving coordinates in WGS84
    points="C:/wamp64/www/sites_localites" #table giving coordinates in WGS84
    ,
    #names_coord=c("Group.1","Group.2") #vector of two values giving 
    names_coord=c("longitude","latitude") #vector of two values giving 
    
      ,
    bm=500 #range of first buffer in meters
    ,
    bl=5000 #range of second buffer in meters  
    ,
    layer="E:/BDALTI/BDALTIV2_2-0_75M_ASC_LAMB93-IGN69_FRANCE_2018-01-15/BDALTIV2/1_DONNEES_LIVRAISON_2018-01-00245/BDALTIV2_MNT_75M_ASC_LAMB93_IGN69_FRANCE"
  )
}