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
#BufferSmall=500
BufferMedium=500
BufferLarge=5000 
#récupération de CLC
Sys.time()
CLC12=shapefile("C:/Users/Yves Bas/Downloads/CLC/CLC12_FR_RGF.shp") # 85 sec
Sys.time()

testH=match(CoordH,names(OccSL))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))


#coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
coordinates(OccSL) <- CoordH
proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84

#CRS.new <- CRS(proj4string(CarthageP))
OccSL_L93=spTransform(OccSL,CRS("+init=epsg:2154"))


BufferM=gBuffer(OccSL_L93,width=BufferMedium,byid=T)
BufferL=gBuffer(OccSL_L93,width=BufferLarge,byid=T)


#I don't do Corine Land Cover habitats for small buffers because CLC is not accurate enough (min pol size >> buffer size)
Sys.time()
SpCLC_M=intersect(BufferM[1,],CLC12) # 0.3 sec / pol
Sys.time()
SpCLC_M=subset(SpCLC_M,is.na(SpCLC_M$CODE_12)==F)

OccSL_L93CLC=OccSL_L93


for (i in 1:nlevels(as.factor(SpCLC_M$CODE_12)))
{
  SpCLC_Mi=subset(SpCLC_M,SpCLC_M$CODE_12==levels(as.factor(SpCLC_M$CODE_12))[i])
  #spplot(SpCLC_Mi,zcol="AREA_HA",col="transparent")
  AreaB=gArea(SpCLC_Mi,byid=T)/BufferMedium^2/pi
  AreaAgg=aggregate(AreaB,by=list(SpCLC_Mi$id),FUN=sum)
  names(AreaAgg)[2]=paste0("SpHC",levels(as.factor(SpCLC_M$CODE_12))[i],"M")
  Sys.time()
  OccSL_L93CLC=merge(OccSL_L93CLC,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
  df = as.data.frame(OccSL_L93CLC)[,ncol(OccSL_L93CLC)]       # extract desired columns into a data.frame
  df[is.na(df)] = 0                             # change values that are zero into NA
  OccSL_L93CLC[1:nrow(OccSL_L93CLC),ncol(OccSL_L93CLC)] = df  # insert result back into spatial points data frame
  print(spplot(OccSL_L93CLC,zcol=names(AreaAgg)[2],main=names(AreaAgg)[2]))
  
}

#select three-levels habitats
testNC=((nchar(names(OccSL_L93CLC))==8)
        &(substr(names(OccSL_L93CLC),1,4)=="SpHC")
        &(substr(names(OccSL_L93CLC),8,8)=="M"))
N3=names(OccSL_L93CLC)[testNC]
HabufPropT3l=as.data.table(OccSL_L93CLC)[,..N3]
Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,6)))

HabufPropT=OccSL_L93CLC$id
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

#OccSL_L93CLC=spCbind(OccSL_L93CLC,HabufPropT)
#for (i in 1:ncol(HabufPropT))
#{
print(names(HabufPropT)[i])

print(spplot(OccSL_L93CLC,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
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
  OccSL_L93CLC=merge(OccSL_L93CLC,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
  df = as.data.frame(OccSL_L93CLC)[,ncol(OccSL_L93CLC)]       # extract desired columns into a data.frame
  df[is.na(df)] = 0                             # change values that are zero into NA
  OccSL_L93CLC[1:nrow(OccSL_L93CLC),ncol(OccSL_L93CLC)] = df  # insert result back into spatial points data frame
  print(spplot(OccSL_L93CLC,zcol=names(AreaAgg)[2],main=names(AreaAgg)[2]))
  
}

#select three-levels habitats
testNC=((nchar(names(OccSL_L93CLC))==8)
        &(substr(names(OccSL_L93CLC),1,4)=="SpHC")
        &(substr(names(OccSL_L93CLC),8,8)=="L"))
N3=names(OccSL_L93CLC)[testNC]
HabufPropT3l=as.data.table(OccSL_L93CLC)[,..N3]
Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,6)))

HabufPropT=OccSL_L93CLC$id
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
#OccSL_L93CLC=spCbind(OccSL_L93CLC,HabufPropT)
#for (i in 1:ncol(HabufPropT))
#{
print(names(HabufPropT)[i])

print(spplot(OccSL_L93CLC,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
#}

#I don't do OCS habitats for large buffers because it's quite lengthy (40 sec / points)
#At this scale, Corine Land Cover is sufficient anyway


Alti=data.frame(cbind(coordinates(OccSL),SpAltiS,SpAltiM,SpAltiL))

fwrite(Alti,paste0(FOccSL,"_Alti.csv"))

coordinates(Alti) <- CoordH

SelCol=sample(c("SpAltiS","SpAltiM","SpAltiL"),1)
spplot(Alti,zcol=SelCol,main=SelCol)

