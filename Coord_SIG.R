library(data.table)
library(rgdal)
library(raster)
library(sp)
library(ggplot2)
library(MASS)
library(rgeos)
library(maptools)
#library(dismo)
#library(adehabitat)
#pour afficher les milisecondes
BufferSmall=50
BufferMedium=500
BufferLarge=5000
LatMin=41.45
LatMax=51.61
LongMin=-5.9
LongMax=9.73
op <- options(digits.secs=3)

#option (if true will compute distance to water course and surface - time consuming)
dist=F #obsolete probably

#args="RandPts__09_11_66_556000_665000_6125000_6202000_3000" #coordinates
args="RandPts__12_30_34_48_2000"
args[2]="C:/Users/Yves Bas/Documents/SIG/Bioclim_5m/" #where bioclim TIFF files are
#récupération des coordonnées dont il faut extraire le SIG
CoordCPL3=fread(paste0("./VigieChiro/GIS/",args[1],".csv"))

#récupération de la couche lumière
ALAN=raster("C:/Users/Yves Bas/Downloads/SVDNB_npp_20161201-20161231_75N060W_vcmslcfg_v10_c201701271138.avg_rade9.tif")
#récupération de la couche habitats et de sa nomenclature
Hab=raster("C:/Users/Yves Bas/Downloads/OCS_2016_CESBIO.tif")
NomHab=fread("C:/Users/Yves Bas/Downloads/Nom_OSO.csv")

#récupération de CLC
Sys.time()
CLC12=shapefile("C:/Users/Yves Bas/Downloads/CLC/CLC12_FR_RGF.shp") # 85 sec
Sys.time()

#récupération des données Carthage (eau)
Sys.time()
CarthageP <- shapefile("C:/Users/Yves Bas/Downloads/CARTHAGE_PLAN/HYDROGRAPHIE_SURFACIQUE.shp") # 11 sec
Sys.time()
CarthageC <- shapefile("C:/Users/Yves Bas/Downloads/CARTHAGE_COURS/TRONCON_HYDROGRAPHIQUE.shp") # 6 min
Sys.time()
#récupération des données biogéo
Biogeo=shapefile("C:/Users/Yves Bas/Documents/SIG/BiogeoRegions2016.shp") # 7 sec
Sys.time()
#récupération de la BD Alti
asc_files<- list.files("C:/Users/Yves Bas/Downloads/BDALTIV2_2-0_75M_ASC_LAMB93-IGN69_FRANCE_2018-01-15/BDALTIV2/1_DONNEES_LIVRAISON_2018-01-00245/BDALTIV2_MNT_75M_ASC_LAMB93_IGN69_FRANCE"
                       ,pattern =".asc$",full.names=T)
Sys.time()





#sélection selon lat/long min/max
CoordCPL3=subset(CoordCPL3,(CoordCPL3$Group.1>LongMin)
                &(CoordCPL3$Group.1<LongMax)
                &(CoordCPL3$Group.2>LatMin)
                &(CoordCPL3$Group.2<LatMax))

CoordCPL3$id=c(1:nrow(CoordCPL3))




#conversion coordonnées en Lambert 93 pour couches habitat et eau
coordinates(CoordCPL3) <- c("Group.1", "Group.2")
proj4string(CoordCPL3) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(CarthageP))
CoordL93=spTransform(CoordCPL3,CRS.new)

#extraction des distances point-eau
#subset des points d'eau douce
CarthagePP=CarthageP[CarthageP$NATURE=="Eau douce permanente",]
#une première distance mini au point d'eau le plus proche
Sys.time()

if(dist){
distPP=vector()
for (i in 1:nrow(CoordL93)) #prend 20 minutes
  #  for (i in 1:10)
{
  distPP=c(distPP,gDistance(CoordL93[i,],CarthagePP))
}
Sys.time()
}else{
  distPP=rep(NA,nrow(CoordL93))
}
#subset des cours d'eau permanent
CarthageCP=CarthageC[CarthageC$ETAT=="Permanent",]

if(dist){
#une deuxième distance mini au cours d'eau le plus proche
Sys.time()
distPC=vector()
for (i in 1:nrow(CoordL93)) #prend 90 minutes
  #    for (i in 1:10)
{
  distPC=c(distPC,gDistance(CoordL93[i,],CarthageCP))
}
Sys.time()

#le min des 2 distances
distEau=(distPP)*(distPP<distPC)+(distPC)*(distPP>=distPC)

#chercher et déterminer un seuil de distance correspondant au "bord de l'eau"
test=subset(distEau,distEau<100)
hist(test,breaks=100)
#50 m semble correspondre à un seuil dans la distribution
#  Cohérent avec
Eau=(distEau<50) 
}else{
  distPP=rep(NA,nrow(CoordL93))
  distEau=rep(NA,nrow(CoordL93))
  Eau=rep(NA,nrow(CoordL93))
    }

#buffers of water surface 
#loop to avoid exceeding memory

BufferS=gBuffer(CoordL93,width=BufferSmall,byid=T)

SpPPlistS=list()
Sys.time()
for (k in 1:ceiling(nrow(CoordL93)/1000))
{
  SpPPlistS[[k]]=intersect(BufferS[((k-1)*1000+1)
                                     :(min(k*1000,nrow(CoordL93))),],CarthagePP) # 0.05 sec / pol
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
CoordL93PP=merge(CoordL93,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
CoordL93PP$SpWS_S[is.na(CoordL93PP$SpWS_S)]=0
spplot(CoordL93PP,zcol="SpWS_S",col="transparent")

}else{
  CoordL93PP=CoordL93
  CoordL93PP$SpWS_S=0
  }

BufferM=gBuffer(CoordL93,width=BufferMedium,byid=T)

SpPPlistS=list()
Sys.time()
for (k in 1:ceiling(nrow(CoordL93)/1000))
{
  SpPPlistS[[k]]=intersect(BufferM[((k-1)*1000+1)
                                   :(min(k*1000,nrow(CoordL93))),],CarthagePP) # 0.05 sec / pol
  print(paste(k,Sys.time()))
}
SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
plot(SpCarthagePP)
AreaB=gArea(SpCarthagePP,byid=T)
AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                  ,FUN=function(x) sum(x)/BufferMedium^2/pi)
names(AreaAgg)[ncol(AreaAgg)]="SpWS_M"
Sys.time()
CoordL93PP=merge(CoordL93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
CoordL93PP$SpWS_M[is.na(CoordL93PP$SpWS_M)]=0
spplot(CoordL93PP,zcol="SpWS_M",col="transparent")

BufferL=gBuffer(CoordL93,width=BufferLarge,byid=T)

SpPPlistS=list()
Sys.time()
for (k in 1:ceiling(nrow(CoordL93)/1000))
{
  SpPPlistS[[k]]=intersect(BufferL[((k-1)*1000+1)
                                   :(min(k*1000,nrow(CoordL93))),],CarthagePP) # 0.05 sec / pol
  print(paste(k,Sys.time()))
}
SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
plot(SpCarthagePP)
AreaB=gArea(SpCarthagePP,byid=T)
AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                  ,FUN=function(x) sum(x)/BufferLarge^2/pi)
names(AreaAgg)[ncol(AreaAgg)]="SpWS_L"
CoordL93PP=merge(CoordL93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
CoordL93PP$SpWS_L[is.na(CoordL93PP$SpWS_L)]=0
spplot(CoordL93PP,zcol="SpWS_L",col="transparent")


#for water courses
Sys.time()
buftemp=intersect(CarthageCP,BufferS) # 0.05 sec / buffer
Sys.time()
LengthB=gLength(buftemp,byid=T)
Sys.time()
PC_50=aggregate(LengthB,by=list(buftemp$id),FUN=sum)
names(PC_50)[ncol(PC_50)]="SpWC_S"
CoordL93PP=merge(CoordL93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
CoordL93PP$SpWC_S[is.na(CoordL93PP$SpWC_S)]=0
spplot(CoordL93PP,zcol="SpWC_S",col="transparent")

Sys.time()
buftemp=intersect(CarthageCP,BufferM) # 0.1 sec / buffer
Sys.time()
LengthB=gLength(buftemp,byid=T)
Sys.time()
PC_50=aggregate(LengthB,by=list(buftemp$id),FUN=sum)
names(PC_50)[ncol(PC_50)]="SpWC_M"
CoordL93PP=merge(CoordL93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
CoordL93PP$SpWC_M[is.na(CoordL93PP$SpWC_M)]=0
spplot(CoordL93PP,zcol="SpWC_M",col="transparent")

Sys.time()
buftemp=intersect(CarthageCP,BufferL) # 0.2 sec / buffer
Sys.time()
LengthB=gLength(buftemp,byid=T)
Sys.time()
PC_50=aggregate(LengthB,by=list(buftemp$id),FUN=sum)
names(PC_50)[ncol(PC_50)]="SpWC_L"
CoordL93PP=merge(CoordL93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
CoordL93PP$SpWC_L[is.na(CoordL93PP$SpWC_L)]=0
spplot(CoordL93PP,zcol="SpWC_L",col="transparent")



#habitats (OCS)
Sys.time()
#DataHp=extract(Hab,CoordL93[,],buffer=BufferSmall) #2 sec / buffer
Sys.time()

#Hab1=rasterFromCells(Hab,11)
Sys.time()
HabufProp=list()
for (i in 1:nrow(CoordL93)) # 2 sec / points
#for (i in 1:80)
{
   radius<-BufferSmall # set the buffer radius
   buf <- buffer(CoordL93[i,], width=radius) # buffer extraction per se
   Sys.time()
   Habuf=extract(Hab,buf)
   Sys.time()
   HabufProp[[i]]=as.data.table(t(as.matrix(table(Habuf))/sum(table(Habuf))))
   if(i%%100==1){print(paste(i,"/",nrow(CoordL93),Sys.time()))}
}

HabufPropT=rbindlist(HabufProp,fill=T)
HabufPropT[is.na(HabufPropT)]=0
Sys.time()

colnames(HabufPropT)=paste0("SpHO",colnames(HabufPropT),"S")

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
  names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"S")
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
    names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"S")
  }
}

CoordL932=spCbind(CoordL93,HabufPropT)

i=sample(c(1:ncol(HabufPropT)),1)
print(names(HabufPropT)[i])
print(spplot(CoordL932,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))



#extract OCS habitats for medium buffers
Sys.time()
HabufProp=list()
for (i in 1:nrow(CoordL93)) # 2 sec / points
  #for (i in 1:80)
{
  radius<-BufferMedium # set the buffer radius
  buf <- buffer(CoordL93[i,], width=radius) # buffer extraction per se
  Sys.time()
  Habuf=extract(Hab,buf)
  Sys.time()
  HabufProp[[i]]=as.data.table(t(as.matrix(table(Habuf))/sum(table(Habuf))))
  if(i%%100==1){print(paste(i,"/",nrow(CoordL93),Sys.time()))}
}

HabufPropT=rbindlist(HabufProp,fill=T)
HabufPropT[is.na(HabufPropT)]=0
Sys.time()

colnames(HabufPropT)=paste0("SpHO",colnames(HabufPropT),"M")

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

CoordL933=spCbind(CoordL932,HabufPropT)

i=sample(c(1:ncol(HabufPropT)),1)
print(names(HabufPropT)[i])
print(spplot(CoordL933,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))


#I don't do OCS habitats for large buffers because it's quite lengthy (40 sec / points)
#At this scale, Corine Land Cover is sufficient anyway
#I don't do Corine Land Cover habitats for small buffers because CLC is not accurate enough (min pol size >> buffer size)
Sys.time()
SpCLC_M=intersect(BufferM,CLC12) # 0.3 sec / pol
Sys.time()
SpCLC_M=subset(SpCLC_M,is.na(SpCLC_M$CODE_12)==F)

CoordL93CLC=CoordL933


for (i in 1:nlevels(as.factor(SpCLC_M$CODE_12)))
{
  SpCLC_Mi=subset(SpCLC_M,SpCLC_M$CODE_12==levels(as.factor(SpCLC_M$CODE_12))[i])
  #spplot(SpCLC_Mi,zcol="AREA_HA",col="transparent")
  AreaB=gArea(SpCLC_Mi,byid=T)/BufferMedium^2/pi
  AreaAgg=aggregate(AreaB,by=list(SpCLC_Mi$id),FUN=sum)
  names(AreaAgg)[2]=paste0("SpHC",levels(as.factor(SpCLC_M$CODE_12))[i],"M")
  Sys.time()
  CoordL93CLC=merge(CoordL93CLC,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
  df = as.data.frame(CoordL93CLC)[,ncol(CoordL93CLC)]       # extract desired columns into a data.frame
  df[is.na(df)] = 0                             # change values that are zero into NA
  CoordL93CLC[1:nrow(CoordL93CLC),ncol(CoordL93CLC)] = df  # insert result back into spatial points data frame
  print(spplot(CoordL93CLC,zcol=names(AreaAgg)[2],main=names(AreaAgg)[2]))
  
}

#select three-levels habitats
testNC=((nchar(names(CoordL93CLC))==8)
        &(substr(names(CoordL93CLC),1,4)=="SpHC")
        &(substr(names(CoordL93CLC),8,8)=="M"))
N3=names(CoordL93CLC)[testNC]
HabufPropT3l=as.data.table(CoordL93CLC)[,..N3]
Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,6)))

HabufPropT=CoordL93CLC$id
if(nrow(Prop32)>0)
{
  #sum 2-levels habitats 
  for (i in 1:nrow(Prop32))
  {
    testi=substr(colnames(HabufPropT3l),1,6)==Prop32$V1[i]
    Ni=colnames(HabufPropT3l)[testi]
    HabufPropTi=HabufPropT3l[,..Ni]
    HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
    HabufPropT=as.data.table(cbind(HabufPropT,HabufPropTsum))
    names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"M")
  }
}

#select 2-levels habitats
testNC=((nchar(names(HabufPropT))==7))
N3=names(HabufPropT)[testNC]
HabufPropT3l=as.data.table(HabufPropT)[,..N3]
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
    HabufPropT=as.data.table(cbind(HabufPropT,HabufPropTsum))
    names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"M")
  }
}

i=sample(c(1:ncol(HabufPropT)),1)

#CoordL93CLC=spCbind(CoordL93CLC,HabufPropT)
#for (i in 1:ncol(HabufPropT))
#{
print(names(HabufPropT)[i])

  print(spplot(CoordL93CLC,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
#}

  
Sys.time()
SpCLC_L0=intersect(BufferL,CLC12) # 0.3 sec / pol
Sys.time()
SpCLC_L=subset(SpCLC_L0,is.na(SpCLC_L0$CODE_12)==F)


for (i in 1:nlevels(as.factor(SpCLC_L$CODE_12)))
{
  SpCLC_Li=subset(SpCLC_L,SpCLC_L$CODE_12==levels(as.factor(SpCLC_L$CODE_12))[i])
  #spplot(SpCLC_Li,zcol="AREA_HA",col="transparent")
  AreaB=gArea(SpCLC_Li,byid=T)/BufferLarge^2/pi
  AreaAgg=aggregate(AreaB,by=list(SpCLC_Li$id),FUN=sum)
  names(AreaAgg)[2]=paste0("SpHC",levels(as.factor(SpCLC_L$CODE_12))[i],"L")
  Sys.time()
  CoordL93CLC=merge(CoordL93CLC,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
  df = as.data.frame(CoordL93CLC)[,ncol(CoordL93CLC)]       # extract desired columns into a data.frame
  df[is.na(df)] = 0                             # change values that are zero into NA
  CoordL93CLC[1:nrow(CoordL93CLC),ncol(CoordL93CLC)] = df  # insert result back into spatial points data frame
  print(spplot(CoordL93CLC,zcol=names(AreaAgg)[2],main=names(AreaAgg)[2]))
  
}

#select three-levels habitats
testNC=((nchar(names(CoordL93CLC))==8)
        &(substr(names(CoordL93CLC),1,4)=="SpHC")
        &(substr(names(CoordL93CLC),8,8)=="L"))
N3=names(CoordL93CLC)[testNC]
HabufPropT3l=as.data.table(CoordL93CLC)[,..N3]
Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,6)))

HabufPropT=CoordL93CLC$id
if(nrow(Prop32)>0)
{
  #sum 2-levels habitats 
  for (i in 1:nrow(Prop32))
  {
    testi=substr(colnames(HabufPropT3l),1,6)==Prop32$V1[i]
    Ni=colnames(HabufPropT3l)[testi]
    HabufPropTi=HabufPropT3l[,..Ni]
    HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
    HabufPropT=as.data.table(cbind(HabufPropT,HabufPropTsum))
    names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"L")
  }
}

#select 2-levels habitats
testNC=((nchar(names(HabufPropT))==7))
N3=names(HabufPropT)[testNC]
HabufPropT3l=as.data.table(HabufPropT)[,..N3]
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
    HabufPropT=as.data.table(cbind(HabufPropT,HabufPropTsum))
    names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"L")
  }
}


i=sample(c(1:ncol(HabufPropT)),1)
#CoordL93CLC=spCbind(CoordL93CLC,HabufPropT)
#for (i in 1:ncol(HabufPropT))
#{
print(names(HabufPropT)[i])

 print(spplot(CoordL93CLC,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
#}



#extraction des zones biogeo
#conversion de coordonnées dans la projection utilisée par l'europe
CRS.eu <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80
+units=m +no_defs")
Coordeu=spTransform(CoordCPL3,CRS.eu)
Sys.time()
DataBG=extract(Biogeo,Coordeu) # 0.03 sec / points
Sys.time()

#données bioclim
ListTifiles=list.files(args[2],full.names=T)

for (i in 2:length(ListTifiles)) #0.1 sec / points
{
  Bioc1=raster(ListTifiles[i])
  Sys.time()
  SpBioci=extract(Bioc1,CoordCPL3)
  Sys.time()
  CoordCPL3=spCbind(CoordCPL3,SpBioci)
  names(CoordCPL3)[ncol(CoordCPL3)]=
    paste0("SpBioc",substr(ListTifiles[i]
                           ,nchar(ListTifiles[i])-5
                           ,nchar(ListTifiles[i])-4))
  print(spplot(CoordCPL3,zcol=names(CoordCPL3)[ncol(CoordCPL3)]
               ,main=names(CoordCPL3)[ncol(CoordCPL3)]))
}


#extraction des valeurs de lumière artif
SpALAN_M=extract(ALAN,CoordCPL3[,1:2])
Sys.time()
CoordCPL3L=spCbind(CoordCPL3,SpALAN_M)
spplot(CoordCPL3L,zcol="SpALAN_M",main="SpALAN_M",col="transparent")

Sys.time()
SpALAN_L=extract(ALAN,CoordCPL3[,1:2],buffer=BufferLarge,fun=mean) # 2 sec / données
Sys.time()
CoordCPL3L=spCbind(CoordCPL3L,SpALAN_L)

spplot(CoordCPL3L,zcol="SpALAN_L",main="SpALAN_L",col="transparent")



#extraction des données alti
rast.list <- list()
for(i in 1:length(asc_files)) { rast.list[i] <- raster(asc_files[i]) }
rast.list$fun <- mean
Sys.time()
Alti <- do.call(mosaic,rast.list) # 8 min
Sys.time()
#plot(Alti)
Sys.time()
#extraction des altitudes ponctuelles
Sys.time()
SpAltiS=extract(Alti,CoordL93) # < 0.001 sec / points
CoordL93HA=spCbind(CoordL93CLC,SpAltiS)
spplot(CoordL93HA,zcol="SpAltiS",main="SpAltiS")

Sys.time()
SpAltiM=extract(Alti,CoordL93,buffer=BufferMedium,fun=mean) # 0.01 sec / points
CoordL93HA=spCbind(CoordL93HA,SpAltiM)
spplot(CoordL93HA,zcol="SpAltiM",main="SpAltiM")

Sys.time()
SpAltiL=extract(Alti,CoordL93[,],buffer=BufferLarge,fun=mean) # 0.02 sec / points
Sys.time()

CoordL93HA=spCbind(CoordL93HA,SpAltiL)
spplot(CoordL93HA,zcol="SpAltiL",main="SpAltiL")


#CoordL93HA (OCS, CLC, Alti)
#CoordCPL3L (Bioclim, ALAN)
#CoordL93PP (Carthage)

CoordL93HAL=merge(CoordL93HA,CoordCPL3L,by="id")
CoordL93HALE=merge(CoordL93HAL,as.data.frame(CoordL93PP),by="id")

#tableau de sortie
fwrite(as.data.table(CoordL93HALE),paste0("./VigieChiro/GIS/GI_"
                                          ,args[1],"_Lat",LatMin
                                          ,"_",LatMax,"_Long"
                                          ,LongMin,"_",LongMax,".csv"))

#writeOGR(CoordL93HALE,dsn="./VigieChiro/GIS/",layer=paste0("CoordSIG_Lat",round(LatMin),"_"
 #                                         ,round(LatMax),"_Long",round(LongMin),"_",round(LongMax),substr(Sys.time(),1,10)),driver="ESRI Shapefile")



