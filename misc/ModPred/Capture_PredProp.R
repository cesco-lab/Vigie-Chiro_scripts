library(data.table)
library(sp)
library(raster)
library(randomForest)
library(spdep)
#library(readxl)
ListGroup=list.files("C:/Users/yvesb/Documents/VigieChiro/GIS/Capture/old2",full.names=T,pattern=".csv$")
GI=fread("C:/Users/yvesb/Downloads/GI_FR_site_capture_chiro.csv")
Ncoord=40
Coordinates=c("X_CENTROID","Y_CENTROID")
DirOut="C:/Users/yvesb/Documents/VigieChiro/ModPred/PA_Capture2203"
#Modulo=9
#dir.create("VigieChiro)
#dir.create("VigieChiro/ModPred)

dir.create(DirOut)

sum(is.na(GI))
GI[is.na(GI)]=0

for (j in 1:length(ListGroup))
{
  print(ListGroup[j])
  DataGroup=fread(ListGroup[j])
  DataMod=unique(DataGroup,by=c(Coordinates,"presence"))
  
  DataGI=merge(DataMod,GI,by=Coordinates)
  summary(DataGI$presence)
  if(max(DataGI$presence)==1)
  {
    #add several rotated coordinates
    CoordDS=as.matrix(subset(DataGI,select=Coordinates))
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
    #print(summary(testNA))
    print(nrow(DataGI))
    
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
    
    if(sd(DataGI$presence)>0)
    {
      Sys.time()
      ModRF=randomForest(x=Predictors,y=as.factor(DataGI$presence)
                         ,replace=T
                         ,importance=F
                         #,sampsize=DownSample2
                         ,ntree=500) #0.1 sec / tree
      Sys.time()
      
      
      #varImpPlot(ModRFNW,cex=0.5,main=paste("Presence",SpTarget))
      #print(paste("TxErreur: ",ModRFNW$confusion[2,3]))
      #varImpPlot(ModRF,cex=0.5,main=paste("Presence",SpTarget))
      #print(paste("TxErreur: ",ModRF$confusion[2,3]))
      #ModRF$confusion
      varImpPlot(ModRF,cex=0.5,main=ListGroup[j])
      print(paste("TxErreur: ",ModRF$confusion[2,3]))
      ModRF$confusion
      
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
      save (ModRF,file=paste0(DirOut,"/ModRFPresence_"
                              ,gsub(".csv","",basename(ListGroup[j])),".learner")) 
    }
  }
}

}
