Test=T

if(exists("Pipeline")){Test=F}

Coord_Carthage=function(points,names_coord,bs,bm,bl,carthagep,carthagec)
{
  
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  library(rgeos)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  CoordH=names_coord
  #library(Rnightlights)
  BufferSmall=bs
  BufferMedium=bm
  BufferLarge=bl 
  #récupération des données Carthage (eau)
  Sys.time()
  CarthageP <- shapefile(carthagep)
  CarthageC <- shapefile(carthagec)
  Split=F
  #Start=10001
  #End=20000
  Start=270001
  End=280194
  
  
  if(Split)
  {
    OccSL=OccSL[Start:(min(End,nrow(OccSL))),]
  }
  
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  OccSL$id=c(1:nrow(OccSL))
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  proj4string(CarthageP) <- CRS("+init=epsg:4326") # WGS 84
  proj4string(CarthageC) <- CRS("+init=epsg:4326") # WGS 84
  CarthageP=spTransform(CarthageP,CRS("+init=epsg:2154"))
  CarthageC=spTransform(CarthageC,CRS("+init=epsg:2154"))
    
  #CRS.new <- CRS(proj4string(CarthageP))
  #OccSL_L93=spTransform(OccSL,CRS(proj4string(CarthageC)))
  OccSL_L93=spTransform(OccSL,CRS("+init=epsg:2154"))
  
  #subset des points d'eau douce
  #CarthagePP=CarthageP[CarthageP$NATURE=="Eau douce permanente",]
  
  #subset des cours d'eau permanent
  #CarthageCP=CarthageC[CarthageC$ETAT=="Permanent",]
  
  ClassP=unique(CarthageP$Nature)
  ClassP=ClassP[order(ClassP)]
  CPd=data.frame(ClassP,Code=c(1:length(ClassP)))
  fwrite(CPd,"CarthageP_dictionary.csv",sep=";")
  
  OccSL_L93PP=OccSL_L93
  for (h in 1:length(ClassP))
  {
    CarthagePP=CarthageP[CarthageP$Nature==ClassP[h],]
    
    #buffers of water surface 
    #loop to avoid exceeding memory
    BufferS=gBuffer(OccSL_L93,width=BufferSmall,byid=T)
    
    SpPPlistS=list()
    Sys.time()
    l=0
    for (k in 1:ceiling(nrow(OccSL_L93)/1000))
    {
      Itemp=intersect(BufferS[((k-1)*1000+1)
                              :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
      print(paste(k,Sys.time()))
      if(length(Itemp)>0)
      {
        l=l+1
        SpPPlistS[[l]]=Itemp
        
      }
    }
    if(length(SpPPlistS)>0)
    {
      test=sapply(SpPPlistS,FUN=function(x) !is.null(x))
    SpPPlistS=subset(SpPPlistS,test)
    }
    SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
    #plot(SpCarthagePP)
    if(length(SpCarthagePP)>0)
    {
      AreaB=gArea(SpCarthagePP,byid=T)
      AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                        ,FUN=function(x) sum(x)/BufferSmall^2/pi)
      names(AreaAgg)[ncol(AreaAgg)]="SpWS_S"
      Sys.time()
      OccSL_L93PP=merge(OccSL_L93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
      OccSL_L93PP$SpWS_S[is.na(OccSL_L93PP$SpWS_S)]=0
      spplot(OccSL_L93PP,zcol="SpWS_S",col="transparent")
      
    }else{
      OccSL_L93PP$SpTemp=0
    }
    names(OccSL_L93PP)[ncol(OccSL_L93PP)]=paste0("SpCP",h,"S")
    
    BufferM=gBuffer(OccSL_L93,width=BufferMedium,byid=T)
    
    SpPPlistS=list()
    Sys.time()
    for (k in 1:ceiling(nrow(OccSL_L93)/1000))
    {
      SpPPlistS[[k]]=intersect(BufferM[((k-1)*1000+1)
                                       :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
      print(paste(k,Sys.time()))
    }
    if(length(SpPPlistS)>0)
    {
      test=sapply(SpPPlistS,FUN=function(x) !is.null(x))
      SpPPlistS=subset(SpPPlistS,test)
    }
    SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
    #plot(SpCarthagePP)
    if(length(SpCarthagePP)>0)
    {
      
      AreaB=gArea(SpCarthagePP,byid=T)
      AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                        ,FUN=function(x) sum(x)/BufferMedium^2/pi)
      names(AreaAgg)[ncol(AreaAgg)]="SpWS_M"
      Sys.time()
      OccSL_L93PP=merge(OccSL_L93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
      OccSL_L93PP$SpWS_M[is.na(OccSL_L93PP$SpWS_M)]=0
      spplot(OccSL_L93PP,zcol="SpWS_M",col="transparent")
    }else{
      OccSL_L93PP$SpWS_M=0
    }
    names(OccSL_L93PP)[ncol(OccSL_L93PP)]=paste0("SpCP",h,"M")
    
    BufferL=gBuffer(OccSL_L93,width=BufferLarge,byid=T)
    
    SpPPlistS=list()
    Sys.time()
    for (k in 1:ceiling(nrow(OccSL_L93)/1000))
    {
      SpPPlistS[[k]]=intersect(BufferL[((k-1)*1000+1)
                                       :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
      print(paste(k,Sys.time()))
    }
    if(length(SpPPlistS)>0)
    {
      test=sapply(SpPPlistS,FUN=function(x) !is.null(x))
      SpPPlistS=subset(SpPPlistS,test)
    }
    SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
    #plot(SpCarthagePP)
    if(length(SpCarthagePP)>0)
    {
      
      AreaB=gArea(SpCarthagePP,byid=T)
      AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                        ,FUN=function(x) sum(x)/BufferLarge^2/pi)
      names(AreaAgg)[ncol(AreaAgg)]="SpWS_L"
      OccSL_L93PP=merge(OccSL_L93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
      OccSL_L93PP$SpWS_L[is.na(OccSL_L93PP$SpWS_L)]=0
      spplot(OccSL_L93PP,zcol="SpWS_L",col="transparent")
    }else{
      OccSL_L93PP$SpWS_L=0
    }
    names(OccSL_L93PP)[ncol(OccSL_L93PP)]=paste0("SpCP",h,"L")
  }
  
  #for water courses
  
  ClassC=unique(CarthageC$Etat)
  ClassC=ClassC[order(ClassC)]
  CCd=data.frame(ClassC,Code=c(1:length(ClassC)))
  fwrite(CCd,"CarthageC_dictionary.csv",sep=";")
  
  for (h in 1:length(ClassC))
  {
    CarthageCP=CarthageC[CarthageC$Etat==ClassC[h],]
    
  
  SpPClistS=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPClistS[[k]]=intersect(CarthageCP,BufferS[((k-1)*1000+1)
                                                :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  if(length(SpPClistS)>0)
  {
    test=sapply(SpPClistS,FUN=function(x) !is.null(x))
    SpPClistS=subset(SpPClistS,test)
  }
  SpCarthagePC=do.call(rbind,SpPClistS) # 0.05 sec / pol
  Sys.time()
  #buftemp=intersect(CarthageCP,BufferS) # 0.05 sec / buffer
  Sys.time()
  if(is.null(SpCarthagePC))
  {
    OccSL_L93PP$SpTemp=0  
  }else{
    
  LengthB=gLength(SpCarthagePC,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpCarthagePC$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpWC_S"
  OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWC_S[is.na(OccSL_L93PP$SpWC_S)]=0
  spplot(OccSL_L93PP,zcol="SpWC_S",col="transparent")
  }
  names(OccSL_L93PP)[ncol(OccSL_L93PP)]=paste0("SpCC",h,"S")
  
  
  SpPClistM=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPClistM[[k]]=intersect(CarthageCP,BufferM[((k-1)*1000+1)
                                                :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  if(length(SpPClistM)>0)
  {
    test=sapply(SpPClistM,FUN=function(x) !is.null(x))
    SpPClistM=subset(SpPClistM,test)
  }  
  SpCarthagePC=do.call(rbind,SpPClistM) # 0.05 sec / pol
  Sys.time()
  #buftemp=intersect(CarthageCP,BufferM) # 0.05 sec / buffer
  Sys.time()
  if(is.null(SpCarthagePC))
  {
    OccSL_L93PP$SpTemp=0  
  }else{
    LengthB=gLength(SpCarthagePC,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpCarthagePC$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpWC_M"
  OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWC_M[is.na(OccSL_L93PP$SpWC_M)]=0
  spplot(OccSL_L93PP,zcol="SpWC_M",col="transparent")
  }
  names(OccSL_L93PP)[ncol(OccSL_L93PP)]=paste0("SpCC",h,"M")
  
  
  
  SpPClistL=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPClistL[[k]]=intersect(CarthageCP,BufferL[((k-1)*1000+1)
                                                :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  
  if(length(SpPClistL)>0)
  {
    test=sapply(SpPClistL,FUN=function(x) !is.null(x))
    SpPClistL=subset(SpPClistL,test)
  }    
  SpCarthagePC=do.call(rbind,SpPClistL) # 0.05 sec / pol
  Sys.time()
  #buftemp=intersect(CarthageCP,BufferL) # 0.05 sec / buffer
  if(is.null(SpCarthagePC))
  {
    OccSL_L93PP$SpTemp=0  
  }else{
    
  LengthB=gLength(SpCarthagePC,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpCarthagePC$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpWC_L"
  OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWC_L[is.na(OccSL_L93PP$SpWC_L)]=0
  spplot(OccSL_L93PP,zcol="SpWC_L",col="transparent")
  }
  names(OccSL_L93PP)[ncol(OccSL_L93PP)]=paste0("SpCC",h,"L")
  
  }
  OccSL_ARajouter=subset(OccSL_L93PP,select=grepl("Sp",names(OccSL_L93PP)))
  
  Carthage=data.frame(cbind(coordinates(OccSL),as.data.frame(OccSL_ARajouter)))
  
  if(Split)
  {
    NewName=paste0(FOccSL,"_Carthage_",Start,"_",End,".csv")
  }else{
    NewName=paste0(FOccSL,"_Carthage.csv")
  }
  
  fwrite(Carthage,NewName)
  
  coordinates(Carthage) <- CoordH
  
  SelCol=sample(names(OccSL_ARajouter),1)
  spplot(Carthage,zcol=SelCol,main=SelCol)
  
}

if(Test)
{
  #for testing
  Coord_Carthage(
    #points="./Vigiechiro/GIS/SysGrid__3e+05" #table giving coordinates in WGS84
    points="C:/wamp64/www/sites_localites" #table giving coordinates in WGS84
    ,
    #  names_coord=c("decimalLongitude","decimalLatitude") #vector of two values giving 
    #names_coord=c("Group.1","Group.2") #vector of two values giving 
    names_coord=c("longitude","latitude") #vector of two values giving 
    
    ,
    bs=50
    ,
    bm=500
    ,
    bl=5000
    ,
    #carthagep="C:/wamp64/www/CARTHAGE_PLAN/HYDROGRAPHIE_SURFACIQUE.shp"
    carthagep="C:/wamp64/www/CARTHAGE_PLAN/EltHydroSurface_FXX.shp"
    ,
    #  carthagec="C:/wamp64/www/CARTHAGE_COURS/TRONCON_HYDROGRAPHIQUE.shp"
    carthagec="C:/wamp64/www/CARTHAGE_COURS/TronconHydrograElt_FXX.shp"
    
  )
}