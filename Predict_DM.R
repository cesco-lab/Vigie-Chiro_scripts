library(data.table)
library(randomForest)
library(spdep)


#args="Tadten"
#args[2]="GI_SysGrid__30_34_2000_Lat41.45_51.61_Long-5.9_9.73"
#args[3]="21/06/2018" #date of prediction
#args[5]=90
#as.numeric(args[11])=40 #number of coordinates projections (must be a division of 360)


load(paste0("./VigieChiro/ModPred/ModRFDecMin_",args[1],"_Seuil",args[5],".learner"))
Sys.time()
CoordSIG=fread(paste0("./VigieChiro/GIS/",args[2],".csv"))
Sys.time()

CoordSIG$SpFDate=yday(as.Date(args[3]
                              ,format="%d/%m/%Y"))

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

PredLoc=predict(ModRF_DM,CoordSIG)
PredAll=predict(ModRF_DM,CoordSIG,predict.all=T)[[2]]
PredErr=apply(PredAll,MARGIN=1,FUN=sd)

coordinates(CoordSIG) <- c("Group.1", "Group.2")
proj4string(CoordSIG) <- CRS("+init=epsg:4326") # WGS 84

CoordSIG$pred=PredLoc
CoordSIG$err=PredErr

spplot(CoordSIG,zcol="pred")
#spplot(CoordSIG,zcol="err")

Coord=as.data.table(CoordSIG)
Coord=subset(Coord,select=c("Group.1","Group.2","pred","err"))

#print(spplot(DataSaison,zcol="pred",main=ListSp[i]))  


FilName=paste0("./VigieChiro/ModPred/"
               ,args[1],"_DM_",substr(args[3],4,5),"_"
               ,args[2])

fwrite(Coord,paste0(FilName,".csv"))

