library(data.table)
library(randomForest)
library(spdep)


if(length(args)<3)
{
args="Pippyg"
args[2]="GI_coordWGS84_DataPF_SpNuit2_Seuil90_Lat42.41_45.23_Long-1.19_5.6"
args[3]="15/06/2018" #date of prediction
args[5]=50 #Seuil
args[11]=40 #number of coordinates projections (must be a division of 360)
ModRF_file=paste0("./VigieChiro/ModPred/ModRFActLog_",args[1],"_Seuil",args[5],".learner")
}

load(ModRF_file)
Sys.time()
CoordSIG=fread(paste0(args[2],".csv"))
Sys.time()

if("SpAltiS" %in% names(CoordSIG))
{
  CoordSIG$SpAltiS[is.na(CoordSIG$SpAltiS)]=0
}
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpBioC1)==F)

CoordSIG$SpGite=0

if(exists("DateG"))
{
  CoordSIG$SpFDate=DateG
}else{
CoordSIG$SpFDate=yday(as.Date(args[3]
                              ,format="%d/%m/%Y"))
}

if(sum(grepl("Group.1.x",names(CoordSIG)))>0)
{
CoordSIG$Group.1=CoordSIG$Group.1.x
CoordSIG$Group.2=CoordSIG$Group.2.x
CoordSIG$Group.1.x=NULL
CoordSIG$Group.1.y=NULL
CoordSIG$Group.2.x=NULL
CoordSIG$Group.2.y=NULL
}
CoordDS=as.matrix(cbind(CoordSIG$Group.1,CoordSIG$Group.2))

for (a in 0:(as.numeric(args[11])-1))
{
  Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))
  #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
  #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
  CoordSIG=cbind(CoordSIG,Coordi[,1])
  names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoord",a)
}

if(grepl("_DS2",ModRF_file)){ModRF=ModRF_morebootstrap}
if(grepl("_Raw",ModRF_file)){ModRF=ModRFNW}


test=match(row.names(ModRF$importance),names(CoordSIG))
MissingVar=subset(row.names(ModRF$importance),is.na(test))
if(length(MissingVar)>0)
{
  for (j in 1:length(MissingVar))
  {
    CoordSIG$temp=0
    names(CoordSIG)[ncol(CoordSIG)]=MissingVar[j]
  }
}


PredLoc=predict(ModRF,CoordSIG,type="prob")
PredAll=predict(ModRF,CoordSIG,predict.all=T)[[2]]
PredErr=apply(PredAll,MARGIN=1,FUN=sd)

coordinates(CoordSIG) <- c("Group.1", "Group.2")
proj4string(CoordSIG) <- CRS("+init=epsg:4326") # WGS 84

CoordSIG$pred=PredLoc[,2]
CoordSIG$err=PredErr


if((min(CoordSIG$pred)!=max(CoordSIG$pred))&nrow(CoordSIG)<10000)
{
  print(spplot(CoordSIG,zcol="pred",main=basename(ModRF_file)))
#spplot(CoordSIG,zcol="err")
}
Coord=as.data.table(CoordSIG)
Coord=subset(Coord,select=c("Group.1","Group.2","pred","err"))

#print(spplot(DataSaison,zcol="pred",main=ListSp[i]))  

if(exists("DateG"))
{
  Mois=g
}else{
  Mois=substr(args[3],4,5)
}

FilName=paste0(dirname(Prefix)
       ,args[1],"_Presence_",Mois,"_"
       ,args[2])

fwrite(Coord,paste0(FilName,".csv"))


