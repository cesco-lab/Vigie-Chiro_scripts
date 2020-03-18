Test=T

##########INPUTS################
#layers : made by Charlotte ROEMER
# points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)
# bs,bm,bl = buffers in meters

" extraction of data"

Coord_eol=function(points,names_coord,bs,bm,bl,layer1,layer2)
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
  
  #r�cup�ration des couches 
  Sys.time()
  R1=shapefile(layer1) 
  Sys.time()
  R2=shapefile(layer2) 
  R3=shapefile(layer3)
  R4=shapefile(layer4)
  R5=shapefile(layer5)
  R6=shapefile(layer6)
  R7=shapefile(layer7)
  R8=shapefile(layer8)
  R9=shapefile(layer9)
  R10=shapefile(layer10)
  R11=shapefile(layer11) 
  Sys.time()
  R12=shapefile(layer12)
  R13=shapefile(layer13)
  Sys.time()
  plot(R8)
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  
  OccSL$id=c(1:nrow(OccSL))
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  #CRS.new <- CRS(proj4string(CarthageP))
  
  proj4string(R1)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R2)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R3)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R4)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R5)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R6)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R7)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R8)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R9)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R10)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R11)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R12)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(R13)<-CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  OccSL_L93=spTransform(OccSL,CRS(proj4string(R1)))
  
  #rename the height variable of the wind turbine
  R4<-data.frame(R4)
  R4$ht_max=R4$hauteur
  R5<-data.frame(R5)
  R5$ht_max=R5$HT_MAX
  R6<-data.frame(R6)
  R6$ht_max=R6$hauteur
  R9<-data.frame(R9)
  R9$ht_max=R9$hauteur
  
  #3 regions do not have data on the height of wind turbines. R8,R10,R12 : Nouvelle-Aquitaine, Grand-Est, Ile de France
  Eol_ht_max=rbind(data.frame(R1)[,c("ht_max","coords.x1","coords.x2")],data.frame(R2)[,c("ht_max","coords.x1","coords.x2")],data.frame(R3)[,c("ht_max","coords.x1","coords.x2")],data.frame(R4)[,c("ht_max","coords.x1","coords.x2")]
                   ,data.frame(R5)[,c("ht_max","coords.x1","coords.x2")],data.frame(R6)[,c("ht_max","coords.x1","coords.x2")],data.frame(R7)[,c("ht_max","coords.x1","coords.x2")],data.frame(R9)[,c("ht_max","coords.x1","coords.x2")]
                   ,data.frame(R11)[,c("ht_max","coords.x1","coords.x2")],data.frame(R13)[,c("ht_max","coords.x1","coords.x2")])
  
  Eol=rbind(data.frame(R1)[,c("coords.x1","coords.x2")],data.frame(R2)[,c("coords.x1","coords.x2")],data.frame(R3)[,c("coords.x1","coords.x2")],data.frame(R4)[,c("coords.x1","coords.x2")],data.frame(R5)[,c("coords.x1","coords.x2")]
            ,data.frame(R6)[,c("coords.x1","coords.x2")],data.frame(R7)[,c("coords.x1","coords.x2")],data.frame(R8)[,c("coords.x1","coords.x2")],data.frame(R9)[,c("coords.x1","coords.x2")],data.frame(R10)[,c("coords.x1","coords.x2")]
            ,data.frame(R11)[,c("coords.x1","coords.x2")],data.frame(R12)[,c("coords.x1","coords.x2")],data.frame(R13)[,c("coords.x1","coords.x2")])
  Eol=data.frame(Eol,x1=rep(1,length(Eol$coords.x1)))
  coordinates(Eol) <- c("coords.x1","coords.x2")
  proj4string(Eol) <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  plot(Eol)
  ##########################################
  ##########################################
  #############  Buffer #  #################
  ##########################################
  ##########################################
  
  ########
  #Buffer S
  ########
  
  BufferS=gBuffer(OccSL_L93,width=BufferSmall,byid=T)
  Sys.time()
  #plot(BufferS)
  
  SpEollistS=intersect(Eol,BufferS) # 0.05 sec / pol
  
  
  SpEol=SpEollistS # 0.05 sec / pol
  
  Sys.time()
  hLengthB=gLength(SpEol,byid=T)
  Sys.time()
  if(length(hLengthB)>0)
  {
    PC_50=aggregate(SpEol$x,by=list(SpEol$id),FUN=sum)
    names(PC_50)[ncol(PC_50)]="SpRo_S"
  OccSL_L93Re=merge(OccSL_L93,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_S[is.na(OccSL_L93Re$SpRo_S)]=0
  spplot(OccSL_L93Re,zcol="SpRo_S",col="transparent")
  
  }else{
    OccSL_L93Re=OccSL_L93
    OccSL_L93Re$SpRo_S=0
  }
  
  
  ########
  #Buffer M
  ########
  BufferM=gBuffer(OccSL_L93,width=BufferMedium,byid=T)
  
  SpEollistM=intersect(Eol,BufferM) # 0.05 sec / pol
  
  
  SpEol=SpEollistM # 0.05 sec / pol
  
  LengthB=gLength(SpEol,byid=T)
  if(length(LengthB)>0)
  {
    
  Sys.time()
    PC_50=aggregate(SpEol$x,by=list(SpEol$id),FUN=sum)
    names(PC_50)[ncol(PC_50)]="SpRo_M"
  OccSL_L93Re=merge(OccSL_L93Re,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_M[is.na(OccSL_L93Re$SpRo_M)]=0
  spplot(OccSL_L93Re,zcol="SpRo_M",col="transparent")
  
  }else{
    OccSL_L93Re$SpRo_M=0
  }
  
  ########
  #Buffer L
  ########
  BufferL=gBuffer(OccSL_L93,width=BufferLarge,byid=T)
  SpEollistL=intersect(Eol,BufferL) # 0.05 sec / pol
  
  
  SpEol=SpEollistL # 0.05 sec / pol
  Sys.time()
  LengthB=gLength(SpEol,byid=T)
  Sys.time()
  if(length(LengthB)>0)
  {
    
  PC_50=aggregate(SpEol$x,by=list(SpEol$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpRo_L"
  OccSL_L93Re=merge(OccSL_L93Re,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_L[is.na(OccSL_L93Re$SpRo_L)]=0
  spplot(OccSL_L93Re,zcol="SpRo_L",col="transparent")
  
  }else{
    OccSL_L93Re$SpRo_L=0
  }
  
  
  
  
  ##########################################
  ##########################################
  #############  Reseaux   #################
  ##########################################
  ##########################################
  
  #
  
  OccSL_ARajouter=subset(OccSL_L93Re,select=grepl("Sp",names(OccSL_L93Re)))
  
  Reseau=data.frame(cbind(coordinates(OccSL),as.data.frame(OccSL_ARajouter)))
  
  NewName=paste0(FOccSL,"_Reseau.csv")
  
  fwrite(Reseau,NewName)
  
  coordinates(Reseau) <- CoordH
  
  SelCol=sample(names(OccSL_ARajouter),1)
  spplot(Reseau,zcol=SelCol,main=SelCol)
  class(Reseau)
  
}

if(Test)
{
  #for testing
  Coord_eol(
    points="./VigieChiro/GIS/SysGrid__10" #table giving coordinates in WGS84
    ,
    names_coord=c("Group.1","Group.2") #vector of two values giving 
    ,
    bs=50
    ,
    bm=500
    ,
    bl=5000
    ,layer1="C:/wamp64/www/eoliennes/Parcs_eoliens_Auvergne-Rhone-Alpes_en_service.shp"
    ,layer2="C:/wamp64/www/eoliennes/Parcs_eoliens_Bourgogne-Franche-Comte_en_service.shp"
    ,layer3="C:/wamp64/www/eoliennes/Parcs_eoliens_Bretagne_en_service.shp"
    ,layer4="C:/wamp64/www/eoliennes/Parcs_eoliens_Corse_en_service.shp"
    ,layer5="C:/wamp64/www/eoliennes/Parcs_eoliens_Centre-Val-de-Loire_en_service.shp"
    ,layer6="C:/wamp64/www/eoliennes/Parcs_eoliens_Corse_en_service.shp"
    ,layer7="C:/wamp64/www/eoliennes/Parcs_eoliens_Hauts-de-France_en_service.shp"
    ,layer8="C:/wamp64/www/eoliennes/Parcs_eoliens_Ile-de-France_en_service.shp"
    ,layer9="C:/wamp64/www/eoliennes/Parcs_eoliens_Normandie_en_service.shp"
    ,layer10="C:/wamp64/www/eoliennes/Parcs_eoliens_Grand-Est_en_service.shp"
    ,layer11="C:/wamp64/www/eoliennes/Parcs_eoliens_Occitanie_en_service.shp"
    ,layer12="C:/wamp64/www/eoliennes/Parcs_eoliens_Nouvelle-Aquitaine_en_service.shp"
    ,layer13="C:/wamp64/www/eoliennes/Parcs_eoliens_Pays-de-la-Loire_en_service.shp"
  )
}

layer1="C:/wamp64/www/eoliennes/Parcs_eoliens_Auvergne-Rhone-Alpes_en_service.shp"
layer2="C:/wamp64/www/eoliennes/Parcs_eoliens_Bourgogne-Franche-Comte_en_service.shp"
layer3="C:/wamp64/www/eoliennes/Parcs_eoliens_Bretagne_en_service.shp"
layer4="C:/wamp64/www/eoliennes/Parcs_eoliens_Corse_en_service.shp"
layer5="C:/wamp64/www/eoliennes/Parcs_eoliens_Centre-Val-de-Loire_en_service.shp"
layer6="C:/wamp64/www/eoliennes/Parcs_eoliens_Corse_en_service.shp"
layer7="C:/wamp64/www/eoliennes/Parcs_eoliens_Hauts-de-France_en_service.shp"
layer8="C:/wamp64/www/eoliennes/Parcs_eoliens_Ile-de-France_en_service.shp"
layer9="C:/wamp64/www/eoliennes/Parcs_eoliens_Normandie_en_service.shp"
layer10="C:/wamp64/www/eoliennes/Parcs_eoliens_Grand-Est_en_service.shp"
layer11="C:/wamp64/www/eoliennes/Parcs_eoliens_Occitanie_en_service.shp"
layer12="C:/wamp64/www/eoliennes/Parcs_eoliens_Nouvelle-Aquitaine_en_service.shp"
layer13="C:/wamp64/www/eoliennes/Parcs_eoliens_Pays-de-la-Loire_en_service.shp"