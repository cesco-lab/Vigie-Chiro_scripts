library(data.table)
library(sp)
library(raster)
library(randomForest)
library(spdep)
library(readxl)
FSpDispo=list.files(".",pattern="GI_PrioCoord",full.names=T)
#FSpDispo=subset(FSpDispo,grepl("PrioCoord",FSpDispo))
PrioTaxa=fread("DataPriorF_57.csv")
SpDispo=gsub(".csv","",paste(tstrsplit(FSpDispo,split="_")[[4]]
              ,tstrsplit(FSpDispo,split="_")[[5]]))
Ncoord=40
GroupSp=read_excel("GroupeSp.xlsx")
DateDir="./VigieChiro/gbifData/DateSp"

op <- options(digits.secs = 3)

LGI=list()
for (h in 1:length(FSpDispo))
{
  LGI[[h]]=fread(FSpDispo[h])
}
GI=rbindlist(LGI,use.names=T,fill=T)
sum(is.na(GI))
GI[is.na(GI)]=0

test=match(SpDispo,PrioTaxa$ListSpValide)
End=max(test)

for (i in 1:End)
{
  GroupName=PrioTaxa$Group[i]
  DateF=fread(paste0(DateDir,"/DateSp_",PrioTaxa$Group[i],".csv"))
  DateSp=subset(DateF,DateF$ListSpValide==PrioTaxa$ListSpValide[i])
  
  SpTarget=PrioTaxa$ListSpValide[i]
  print(paste(SpTarget,GroupName))
  #LimitF="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
  ListGroup=list.files("./VigieChiro/gbifData/DataGroup",pattern=GroupName
                       ,full.names=T,recursive=T)
  
  GroupC=subset(GroupSp,GroupSp$Group==GroupName)
if(!is.na(GroupC$Complement))
{
    ListGroupC=list.files("./VigieChiro/gbifData/DataGroup"
                          ,pattern=GroupC$Complement
                        ,full.names=T)
ListGroup=c(ListGroup,ListGroupC)
    }
  
  my.data=list()
  for (j in 1:length(ListGroup))
  {
    my.data[[j]]=fread(ListGroup[j])
  }
  
  DataGroup=rbindlist(my.data,fill=T,use.names=T)
  
  DataGroup$presence=as.numeric(DataGroup$name==SpTarget)
  
  DataGroup=subset(DataGroup,DataGroup$yday>DateSp$PicSp[1]-30)
  DataGroup=subset(DataGroup,DataGroup$yday<DateSp$PicSp[1]+30)
  
  DataMod=unique(DataGroup,by=c("decimalLongitude","decimalLatitude","presence"))
  
  DataGI=merge(DataMod,GI,by=c("decimalLongitude","decimalLatitude"))
  summary(DataGI$presence)
  if(max(DataGI$presence)==1)
  {
  #add several rotated coordinates
  CoordDS=as.matrix(cbind(as.data.frame(DataGI$decimalLongitude)
                          ,as.data.frame(DataGI$decimalLatitude)))
  for (a in 0:(Ncoord-1))
  {
    Coordi=Rotation(CoordDS,angle=pi*a/Ncoord)
    #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
    #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
    DataGI=cbind(DataGI,Coordi[,1])
    names(DataGI)[ncol(DataGI)]=paste0("SpCoord",a)
  }
  
  testPred=(substr(names(DataGI),1,2)=="Sp")
  Prednames=names(DataGI)[testPred]
  Predictors=DataGI[,..Prednames]
  
  
  testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
  print(summary(testNA))
  print(nrow(DataGI))
  
  #Npos=sum(DataGI$presence)
  #DownSample=max(Npos,1000)
  NumSample=as.vector(table(DataGI$presence))
  DownSample=pmin(NumSample,1000)
  DownSample2=pmax(round(DownSample/2),1)
  
  #Sys.time()
  #ModRFNW=randomForest(x=Predictors,y=as.factor(DataGI$presence)
   #                    ,replace=T
    #                   ,importance=T
     #                  ,ntree=500) #0.1 sec / tree
  #Sys.time()
  
  #Sys.time()
  #ModRF=randomForest(x=Predictors,y=as.factor(DataGI$presence)
   #                  ,replace=T
    #                 ,importance=T
     #                ,sampsize=DownSample
      #               ,ntree=500) #0.1 sec / tree
  Sys.time()
  ModRF_morebootstrap=randomForest(x=Predictors,y=as.factor(DataGI$presence)
                                   ,replace=T
                                   ,importance=F
                                   ,sampsize=DownSample2
                                   ,ntree=500
                                   ) #0.1 sec / tree
  Sys.time()
  
  
  #varImpPlot(ModRFNW,cex=0.5,main=paste("Presence",SpTarget))
  #print(paste("TxErreur: ",ModRFNW$confusion[2,3]))
  #varImpPlot(ModRF,cex=0.5,main=paste("Presence",SpTarget))
  #print(paste("TxErreur: ",ModRF$confusion[2,3]))
  #ModRF$confusion
  varImpPlot(ModRF_morebootstrap,cex=0.5,main=paste("Presence",SpTarget))
  print(paste("TxErreur: ",ModRF_morebootstrap$confusion[2,3]))
  ModRF_morebootstrap$confusion
  
  #coordinates(DataGI) <- c("Group.1", "Group.2")
  #proj4string(DataGI) <- CRS("+init=epsg:4326") # WGS 84
  #DataGI$pred=ModRF$predicted
  #print(spplot(DataGI,zcol="pred",main=SpTarget))  
  
  
  #save (ModRFNW,file=paste0("./VigieChiro/ModPred/ModRFPresence_",SpTarget,"_"
   #                         ,GroupName,"_"
    #                        ,"Raw.learner")) 
  #save (ModRF,file=paste0("./VigieChiro/ModPred/ModRFPresence_",SpTarget,"_"
   #                       ,GroupName,"_"
    #                      ,"DSnorm.learner")) 
  save (ModRF_morebootstrap,file=paste0("./VigieChiro/ModPred/ModRFPresence_"
                                        ,SpTarget,"_"
                                        ,GroupName,"_"
                                        ,"DS2.learner")) 
  
  
}

}
