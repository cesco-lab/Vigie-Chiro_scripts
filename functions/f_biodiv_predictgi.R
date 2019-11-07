library(data.table)
library(randomForest)
library(spdep)


GIToBePredicted="./VigieChiro/GIS/GI_SysGrid__20000.csv"
DateToBePredicted="15/06/2018" #date of prediction
NumCoord=40 #number of coordinates projections (must be a division of 360)
ModRF_file="./VigieChiro/ModPred/ModRF_nb_contacts_DataSpSL_Urosp_90_Data.csv.learner"

load(ModRF_file)
Sys.time()
CoordSIG=fread(GIToBePredicted)
Sys.time()

CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpAltiS)==F)
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpBioC1)==F)


CoordSIG$SpGite=0
CoordSIG$SpFDate=yday(as.Date(DateToBePredicted
                              ,format="%d/%m/%Y"))

CoordDS=as.matrix(cbind(CoordSIG$Group.1,CoordSIG$Group.2))

for (a in 0:(as.numeric(NumCoord)-1))
{
  Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(NumCoord))
  #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
  #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
  CoordSIG=cbind(CoordSIG,Coordi[,1])
  names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoord",a)
}

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


PredLoc=predict(ModRF,CoordSIG)
PredAll=predict(ModRF,CoordSIG,predict.all=T)[[2]]
PredErr=apply(PredAll,MARGIN=1,FUN=sd)

coordinates(CoordSIG) <- c("Group.1", "Group.2")
proj4string(CoordSIG) <- CRS("+init=epsg:4326") # WGS 84

CoordSIG$pred=PredLoc
CoordSIG$err=PredErr

spplot(CoordSIG,zcol="pred",main=basename(ModRF_file))
#spplot(CoordSIG,zcol="err")

Coord=as.data.table(CoordSIG)
Coord=subset(Coord,select=c("Group.1","Group.2","pred","err"))

#print(spplot(DataSaison,zcol="pred",main=ListSp[i]))  

FilName=paste0("./VigieChiro/ModPred/Sp_"
       ,gsub(".learner","",basename(ModRF_file))
       ,basename(GIToBePredicted))

fwrite(Coord,paste0(FilName,".csv"))


