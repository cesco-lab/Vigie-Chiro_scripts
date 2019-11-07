#TO BE MODIFIED TO TAKE INTO ACCOUNT OTHER DATE FORMATS
library(data.table)
library(rgdal)
library(raster)
library(rgeos)
library(latticeExtra)
library(randomForest)
library(gdata)
library(spdep)
op <- options(digits.secs=3)


FData="./VigieChiro/DataSp/DataSpSL_Urosp_90_Data.csv"
FGI="./VigieChiro/GIS/GI_coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv"
DataLongLat=c("longitude","latitude")
GILongLat=c("Group.1","Group.2")
Metric="nb_contacts"
DateName="date_debut"
NumCoord=40
DM=F
LogTransform=T
Strata=c("id_site","localite")

#recupération des données chiros
DataCPL3=fread(FData)

#recup gridSIG
Sys.time()
CoordSIG=fread(FGI)
Sys.time()

CCSIG=subset(CoordSIG,select=GILongLat)
names(CCSIG)=c("LongPourMerge","LatPourMerge")
CoordSIG=cbind(CoordSIG,CCSIG)

CData=subset(DataCPL3,select=DataLongLat)
names(CData)=c("LongPourMerge","LatPourMerge")
DataCPL3=cbind(DataCPL3,CData)
DataCPL3$Metric=subset(DataCPL3,select=Metric)


CoordPS=merge(DataCPL3,CoordSIG,by=c("LongPourMerge","LatPourMerge"))

DataSaison=CoordPS

#add date of year
if(!is.na(DateName)) #TO BE MODIFIED TO TAKE INTO ACCOUNT OTHER DATE FORMATS
{
  Date0=subset(DataSaison,select=DateName)
  Date1=as.Date(substr(as.data.frame(Date0)[,1],1,10)
                ,format="%d/%m/%Y")
  Date2=yday(Date1)
  DateTrigo=Date2/365*pi*2
  DataSaison$SpFDateC=cos(DateTrigo)
  DataSaison$SpFDateS=sin(DateTrigo)
  
}else{
  DataSaison$SpFDateC=0
  DataSaison$SpFDateS=0
}
#add several rotated coordinates
CoordDS=as.matrix(cbind(DataSaison$LongPourMerge,DataSaison$LongPourMerge))

for (a in 0:(as.numeric(NumCoord)-1))
{
  Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(NumCoord))
  #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
  #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
  DataSaison=cbind(DataSaison,Coordi[,1])
  names(DataSaison)[ncol(DataSaison)]=paste0("SpCoord",a)
}

if(var(DataSaison$Metric)>0)
{
  testPred=(substr(names(DataSaison),1,2)=="Sp")
  Prednames=names(DataSaison)[testPred]
  Predictors=DataSaison[,..Prednames]
  
  
  testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
  
  if(LogTransform)
  {
    DataSaison$MetricToBePredicted=log10(DataSaison$Metric+1)
  }else{
    DataSaison$MetricToBePredicted=DataSaison$Metric
  }
  
  if(is.na(Strata))
  {
    ModRF=randomForest(x=Predictors,y=DataSaison$MetricToBePredicted
                       ,replace=T
                       ,importance=T
    ) #1 sec / tree (for 8000 records, 240 predictors)  
  }else{
    Sys.time()
    StrataTab=subset(DataSaison,select=Strata)
    StrataChar=rep("",nrow(StrataTab))
    for (i in 1:ncol(StrataTab))
    {
      StrataChar=paste(StrataChar,as.data.frame(StrataTab)[,i],sep="_")
    }
    StrataChar=as.factor(StrataChar)
    ModRF=randomForest(x=Predictors,y=DataSaison$MetricToBePredicted
                       ,replace=T
                       ,strata=StrataChar
                       ,importance=T
                       #,ntree=1
    ) #1 sec / tree (for 8000 records, 240 predictors)
    Sys.time()
  }
  
  
  
  varImpPlot(ModRF,cex=0.5,main=gsub(".csv","",basename(FData)))
  print(paste("PseudoR2: ",ModRF$rsq[ModRF$ntree]))
  #coordinates(DataSaison) <- c("Group.1", "Group.2")
  #proj4string(DataSaison) <- CRS("+init=epsg:4326") # WGS 84
  #DataSaison$pred=ModRF$predicted
  #print(spplot(DataSaison,zcol="pred",main=ListSp[i]))  
  
  #test if species is a bat
  save (ModRF,file=paste0("./VigieChiro/ModPred/ModRF_",Metric,"_",basename(FData)
                          ,".learner")) 
}


