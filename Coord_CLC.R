library(data.table)
library(sp)
library(raster)
library(maptools)
library(rgeos)

#library(Rnightlights)
#OccSL=fread("./vigiechiro/Traits/GBIF/OccSL_bush-cricket.csv")
FOccSL="./vigiechiro/GIS/coordWGS84_ALL"
OccSL=fread(paste0(FOccSL,".csv"))
CoordH=c("Group.1", "Group.2")
#CoordH=c("decimalLongitude", "decimalLatitude")
#BufferSmall=500
BufferMedium=500
BufferLarge=5000 
#récupération de CLC
Sys.time()
CLC12=shapefile("C:/Users/Yves Bas/Downloads/CLC/CLC12_FR_RGF.shp") # 85 sec
Sys.time()
#Split=F
#Start=1
#End=10000

testH=match(CoordH,names(OccSL))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))

test=as.data.frame(OccSL)[,testH[1]]
OccSL=OccSL[order(test)]
test2=as.data.frame(OccSL)[,testH[2]]
OccSL=OccSL[order(test2)]

OccSL$id=c(1:nrow(OccSL))

#coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
coordinates(OccSL) <- CoordH
proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84

#CRS.new <- CRS(proj4string(CarthageP))
OccSL_L93=spTransform(OccSL,CRS(proj4string(CLC12)))
test=over(OccSL_L93,CLC12)
OccSL_L93=subset(OccSL_L93,!is.na(test$AREA_HA))
Sys.time()
OccSL=subset(OccSL,OccSL$id %in% OccSL_L93$id)



BufferM=gBuffer(OccSL_L93,width=BufferMedium,byid=T)
BufferL=gBuffer(OccSL_L93,width=BufferLarge,byid=T)


OccSL_L93CLC=OccSL_L93

if(exists("SpCLC_Mtot")){rm(SpCLC_Mtot)}
for (h in 1:ceiling(nrow(BufferM)/300)) #300 is optimal number of buffers for intersection
{
#I don't do Corine Land Cover habitats for small buffers because CLC is not accurate enough (min pol size >> buffer size)
print(paste(h,Sys.time()))
SpCLC_M=intersect(BufferM[((h-1)*300+1):(min(h*300,nrow(BufferM))),],CLC12) # 0.2 sec / pol
print(plot(BufferM[((h-1)*300+1):(min(h*300,nrow(BufferM))),]))
SpCLC_M=subset(SpCLC_M,is.na(SpCLC_M$CODE_12)==F)
if(exists("SpCLC_Mtot")){SpCLC_Mtot=rbind(SpCLC_Mtot,SpCLC_M)}else{SpCLC_Mtot=SpCLC_M}
}

for (i in 1:nlevels(as.factor(SpCLC_Mtot$CODE_12)))
{
  SpCLC_Mtoti=subset(SpCLC_Mtot,SpCLC_Mtot$CODE_12==levels(as.factor(SpCLC_Mtot$CODE_12))[i])
  #spplot(SpCLC_Mtoti,zcol="AREA_HA",col="transparent")
  AreaB=gArea(SpCLC_Mtoti,byid=T)/BufferMedium^2/pi
  AreaAgg=aggregate(AreaB,by=list(SpCLC_Mtoti$id),FUN=sum)
  names(AreaAgg)[2]=paste0("SpHC",levels(as.factor(SpCLC_Mtot$CODE_12))[i],"M")
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

OccSL_L93CLC=spCbind(OccSL_L93CLC,HabufPropT)
#for (i in 1:ncol(HabufPropT))
#{
print(names(HabufPropT)[i])

print(spplot(OccSL_L93CLC,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
#}



if(exists("SpCLC_Ltot")){rm(SpCLC_Ltot)}
for (h in 1:ceiling(nrow(BufferL)/100)) #100 is optimal number of buffers for intersection
{
  #I don't do Corine Land Cover habitats for small buffers because CLC is not accurate enough (min pol size >> buffer size)
  print(paste(h,Sys.time()))
  SpCLC_L=intersect(BufferL[((h-1)*100+1):(min(h*100,nrow(BufferL))),],CLC12) # 0.2 sec / pol
  print(plot(BufferL[((h-1)*100+1):(min(h*100,nrow(BufferL))),]))
  SpCLC_L=subset(SpCLC_L,is.na(SpCLC_L$CODE_12)==F)
  if(exists("SpCLC_Ltot")){SpCLC_Ltot=rbind(SpCLC_Ltot,SpCLC_L)}else{SpCLC_Ltot=SpCLC_L}
}

test=subset(SpCLC_Ltot,SpCLC_Ltot$CODE_12=="242")


for (i in 1:nlevels(as.factor(SpCLC_Ltot$CODE_12)))
{
  SpCLC_Ltoti=subset(SpCLC_Ltot,SpCLC_Ltot$CODE_12==levels(as.factor(SpCLC_Ltot$CODE_12))[i])
  #spplot(SpCLC_Ltoti,zcol="AREA_HA",col="transparent")
  AreaB=gArea(SpCLC_Ltoti,byid=T)/BufferLarge^2/pi
  AreaAgg=aggregate(AreaB,by=list(SpCLC_Ltoti$id),FUN=sum)
  names(AreaAgg)[2]=paste0("SpHC",levels(as.factor(SpCLC_Ltot$CODE_12))[i],"L")
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
OccSL_L93CLC=spCbind(OccSL_L93CLC,HabufPropT)
#for (i in 1:ncol(HabufPropT))
#{
print(names(HabufPropT)[i])

print(spplot(OccSL_L93CLC,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
#}

#I don't do OCS habitats for large buffers because it's quite lengthy (40 sec / points)
#At this scale, Corine Land Cover is sufficient anyway


OccSL_ARajouter=subset(OccSL_L93CLC,select=grepl("Sp",names(OccSL_L93CLC)))

CLC=data.frame(cbind(coordinates(OccSL),as.data.frame(OccSL_ARajouter)))
#if(Split)
#{
#  NewName=paste0(FOccSL,"_CLC_",Start,"_",End,".csv")
#}else{
  NewName=paste0(FOccSL,"_CLC.csv")
#}
fwrite(CLC,NewName)

coordinates(CLC) <- CoordH

SelCol=sample(names(OccSL_ARajouter),1)
spplot(CLC,zcol=SelCol,main=SelCol)

