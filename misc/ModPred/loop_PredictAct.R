library(data.table)
library(randomForest)
library(spdep)

ListDate=c("15/04/2021","15/05/2021","15/06/2021","15/07/2021","15/08/2021","15/09/2021","15/10/2021")
ListSp=c("BarbarVC50","EptserVC50","NycleiVC50","NycnocVC50","MyonatVC50","PipkuhVC50","PipnatVC50"
         ,"PippipVC50","PleausVC50")
DataGI="./GI_Sites_Veolia2008.csv"



predict_Act=function(espece,date=15/07/2021,suffix="_PG",GI,ncoord=40,output="."){
#args="Pippyg"
#GI="GI_coordWGS84_DataPF_SpNuit2_Seuil90_Lat42.41_45.23_Long-1.19_5.6.csv"
#date="15/06/2018" #date of prediction
#suffix=50
#ncoord=40 #number of coordinates projections (must be a division of 360)
#ModRF_file=paste0("./VigieChiro/ModPred/ModRFActLog_",espece,"_Seuil",suffix,".learner")
ModRF_file=paste0("./Act/ModRFActLog_",espece,suffix,".learner")
print(getwd())
#load(paste0("./VigieChiro/ModPred/ModRFActLog_",espece,"_Seuil",suffix,".learner"))
print("L14")
load(ModRF_file)
Sys.time()
CoordSIG=fread(GI)
Sys.time()
print(nrow(CoordSIG))
print(apply(CoordSIG,MARGIN=2,FUN=function(x) sum(is.na(x))))

#print(apply(CoordSIG,MARGIN=1,FUN=function(x) sum(is.na(x))))

CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpAltiS)==F)
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpBioC1)==F)
print(nrow(CoordSIG))
print("M22")

CoordSIG$SpGite=0
CoordSIG$SpFDate=yday(as.Date(date
                              ,format="%d/%m/%Y"))
CoordSIG$SpCDate=cos(CoordSIG$SpFDate/365*2*pi)
CoordSIG$SpSDate=sin(CoordSIG$SpFDate/365*2*pi)
                                                            

if(exists("YearEffect"))
{
if(YearEffect)
{
CoordSIG$SpYear=as.numeric(substr(date,7,10))
}
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
if(!grepl("Group.1",names(CoordSIG))) #dirty
{
CoordSIG$Group.1=CoordSIG$longitude
CoordSIG$Group.2=CoordSIG$latitude
}
CoordDS=as.matrix(cbind(CoordSIG$Group.1,CoordSIG$Group.2))

for (a in 0:(as.numeric(ncoord)-1))
{
  Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(ncoord))
  #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
  #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
  CoordSIG=cbind(CoordSIG,Coordi[,1])
  names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoord",a)
}

test=match(row.names(ModRF$importance),names(CoordSIG))
MissingVar=subset(row.names(ModRF$importance),is.na(test))
print("missing:")
print(MissingVar)
if(length(MissingVar)>0)
{
  for (j in 1:length(MissingVar))
  {
    CoordSIG$temp=0
    names(CoordSIG)[ncol(CoordSIG)]=MissingVar[j]
  }
}
CoordSIG[is.na(CoordSIG)]=0
#print(apply(CoordSIG,MARGIN=2,FUN=function(x) sum(is.na(x))))
print("M61")
PredLoc=predict(ModRF,CoordSIG)
print("M63")
PredAll=predict(ModRF,CoordSIG,predict.all=T)[[2]]
print("M65")
PredErr=apply(PredAll,MARGIN=1,FUN=sd)
print("M67")
coordinates(CoordSIG) <- c("Group.1", "Group.2")
proj4string(CoordSIG) <- CRS("+init=epsg:4326") # WGS 84

CoordSIG$pred=PredLoc
CoordSIG$err=PredErr

#spplot(CoordSIG,zcol="pred",main=espece)
#spplot(CoordSIG,zcol="err")

Coord=as.data.table(CoordSIG)
Coord=subset(Coord,select=c("Group.1","Group.2","pred","err"))
print("M76")
DateForFile=gsub("/","_",date)
DateForFile=substr(DateForFile,1,6)


FilName=paste0(Output,"/"
       ,espece,"_Act_",DateForFile
       ,basename(GI))

print(FilName)

fwrite(Coord,FilName,sep=";")


}

for (i in 1:length(ListSp))
{
print(ListSp[i])
    for (j in 1:length(ListDate))
  {
    predict_Act(espece=ListSp[i],date=ListDate[j],GI=DataGI)    
      print(ListDate[j])
        }
  
}
