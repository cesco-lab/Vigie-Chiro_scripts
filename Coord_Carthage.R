library(data.table)
library(sp)
library(raster)
library(maptools)
library(rgeos)

#library(Rnightlights)
#OccSL=fread("./vigiechiro/Traits/GBIF/OccSL_bush-cricket.csv")
FOccSL="./vigiechiro/GIS/carre_stoc"
OccSL=fread(paste0(FOccSL,".csv"))
CoordH=c("Group.1", "Group.2")
BufferSmall=50
BufferMedium=500
BufferLarge=5000 
#récupération des données Carthage (eau)
Sys.time()
CarthageP <- shapefile("C:/Users/Yves Bas/Downloads/CARTHAGE_PLAN/HYDROGRAPHIE_SURFACIQUE.shp") # 11 sec
Sys.time()
CarthageC <- shapefile("C:/Users/Yves Bas/Downloads/CARTHAGE_COURS/TRONCON_HYDROGRAPHIQUE.shp") # 6 min
Sys.time()


testH=match(CoordH,names(OccSL))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
OccSL$id=c(1:nrow(OccSL))

#coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
coordinates(OccSL) <- CoordH
proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84

#CRS.new <- CRS(proj4string(CarthageP))
OccSL_L93=spTransform(OccSL,CRS("+init=epsg:2154"))

#subset des points d'eau douce
CarthagePP=CarthageP[CarthageP$NATURE=="Eau douce permanente",]
#subset des cours d'eau permanent
CarthageCP=CarthageC[CarthageC$ETAT=="Permanent",]

#buffers of water surface 
#loop to avoid exceeding memory

BufferS=gBuffer(OccSL_L93,width=BufferSmall,byid=T)

SpPPlistS=list()
Sys.time()
for (k in 1:ceiling(nrow(OccSL_L93)/1000))
{
  SpPPlistS[[k]]=intersect(BufferS[((k-1)*1000+1)
                                   :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
  print(paste(k,Sys.time()))
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
  OccSL_L93PP=merge(OccSL_L93,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWS_S[is.na(OccSL_L93PP$SpWS_S)]=0
  spplot(OccSL_L93PP,zcol="SpWS_S",col="transparent")
  
}else{
  OccSL_L93PP=OccSL_L93
  OccSL_L93PP$SpWS_S=0
}

BufferM=gBuffer(OccSL_L93,width=BufferMedium,byid=T)

SpPPlistS=list()
Sys.time()
for (k in 1:ceiling(nrow(OccSL_L93)/1000))
{
  SpPPlistS[[k]]=intersect(BufferM[((k-1)*1000+1)
                                   :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
  print(paste(k,Sys.time()))
}
SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
plot(SpCarthagePP)
AreaB=gArea(SpCarthagePP,byid=T)
AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                  ,FUN=function(x) sum(x)/BufferMedium^2/pi)
names(AreaAgg)[ncol(AreaAgg)]="SpWS_M"
Sys.time()
OccSL_L93PP=merge(OccSL_L93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
OccSL_L93PP$SpWS_M[is.na(OccSL_L93PP$SpWS_M)]=0
spplot(OccSL_L93PP,zcol="SpWS_M",col="transparent")

BufferL=gBuffer(OccSL_L93,width=BufferLarge,byid=T)

SpPPlistS=list()
Sys.time()
for (k in 1:ceiling(nrow(OccSL_L93)/1000))
{
  SpPPlistS[[k]]=intersect(BufferL[((k-1)*1000+1)
                                   :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
  print(paste(k,Sys.time()))
}
SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
plot(SpCarthagePP)
AreaB=gArea(SpCarthagePP,byid=T)
AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                  ,FUN=function(x) sum(x)/BufferLarge^2/pi)
names(AreaAgg)[ncol(AreaAgg)]="SpWS_L"
OccSL_L93PP=merge(OccSL_L93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
OccSL_L93PP$SpWS_L[is.na(OccSL_L93PP$SpWS_L)]=0
spplot(OccSL_L93PP,zcol="SpWS_L",col="transparent")


#for water courses
Sys.time()
buftemp=intersect(CarthageCP,BufferS) # 0.05 sec / buffer
Sys.time()
LengthB=gLength(buftemp,byid=T)
Sys.time()
PC_50=aggregate(LengthB,by=list(buftemp$id),FUN=sum)
names(PC_50)[ncol(PC_50)]="SpWC_S"
OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
OccSL_L93PP$SpWC_S[is.na(OccSL_L93PP$SpWC_S)]=0
spplot(OccSL_L93PP,zcol="SpWC_S",col="transparent")

Sys.time()
buftemp=intersect(CarthageCP,BufferM) # 0.1 sec / buffer
Sys.time()
LengthB=gLength(buftemp,byid=T)
Sys.time()
PC_50=aggregate(LengthB,by=list(buftemp$id),FUN=sum)
names(PC_50)[ncol(PC_50)]="SpWC_M"
OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
OccSL_L93PP$SpWC_M[is.na(OccSL_L93PP$SpWC_M)]=0
spplot(OccSL_L93PP,zcol="SpWC_M",col="transparent")

Sys.time()
buftemp=intersect(CarthageCP,BufferL) # 0.2 sec / buffer
Sys.time()
LengthB=gLength(buftemp,byid=T)
Sys.time()
PC_50=aggregate(LengthB,by=list(buftemp$id),FUN=sum)
names(PC_50)[ncol(PC_50)]="SpWC_L"
OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
OccSL_L93PP$SpWC_L[is.na(OccSL_L93PP$SpWC_L)]=0
spplot(OccSL_L93PP,zcol="SpWC_L",col="transparent")

OccSL_ARajouter=subset(OccSL_L93PP,select=grepl("Sp",names(OccSL_L93PP)))
  
Carthage=data.frame(cbind(coordinates(OccSL),as.data.frame(OccSL_ARajouter)))

fwrite(Carthage,paste0(FOccSL,"_Carthage.csv"))

coordinates(Carthage) <- CoordH

SelCol=sample(names(OccSL_ARajouter),1)
spplot(Carthage,zcol=SelCol,main=SelCol)

