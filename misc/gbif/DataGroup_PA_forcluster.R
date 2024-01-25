library(data.table)
library(sf)
library(randomForest)
library(spdep)
#library(readxl)
OccDir="C:/Users/ybas/Documents/VigieChiro/gbifData/RawTest"
Ncoord=40
GroupSp=fread("C:/Users/ybas/Documents/GroupeSp2.csv")
Modulo=9
#dir.create("VigieChiro)
print(Modulo)
#dir.create("VigieChiro/ModPred)
#patternSelF=c("2020-11-","2020-12-")
#patternSelF2=c("FR","IT","AD","ES","CH")
#patternSelF2=""
#DateDir="C:/Users/ybas/Documents/VigieChiro/gbifData/DateSpAll"
DateData=fread("C:/Users/ybas/Documents/VigieChiro/gbifData/DateSel.csv")
PhylumList=c("Chordata","Arthropoda","Mollusca","Tracheophyta"
             ,"Basidiomycota")
DirOut="C:/Users/ybas/Documents/VigieChiro/gbifData/PredPres"

dir.create(DirOut)

FSpDispo=list.files(OccDir,pattern="GI_",full.names=T,recursive=T)


LGI=list()
Summ=list()
for (h in 1:length(FSpDispo))
{
  LGI[[h]]=fread(FSpDispo[h])
  Zoneh=gsub("GI_","",basename(FSpDispo[h]))
  Zoneh=gsub("_simplified_shortened.csv","",Zoneh)
  LGI[[h]]$zone=Zoneh
  Summh=gsub("GI_","",FSpDispo[h])
  Summh=gsub("_shortened.csv","_summary.csv",Summh)
  Summ[[h]]=fread(Summh)
  Summ[[h]]$zone=Zoneh
}
GI=rbindlist(LGI,use.names=T,fill=T)
Summaries=rbindlist(Summ,use.names=T,fill=T)

SummStrata=paste(Summaries$zone,Summaries$Group.1,Summaries$Group.2,Summaries$Group.3
                 ,"0")


print(sum(is.na(GI)))
GI$yday=as.numeric(substr(GI$eventDate,9,10))+
  (as.numeric(substr(GI$eventDate,6,7))-1)*30
GI2_FirstMonth=subset(GI,GI$yday<31)
GI2_FirstMonth$yday=GI2_FirstMonth$yday+360
GI=rbind(GI,GI2_FirstMonth)
GIsave=GI
ListSpGI=unique(GI$species)
DateData=subset(DateData,DateData$ListSpValide %in% ListSpGI)
DateData=DateData[order(DateData$Order),]
#for (i in seq(from=55690+Modulo,to=End,by=10))
#for (i in seq(from=1,to=End,by=1))
for (i in 1:nrow(DateData))
{
  SpTarget=DateData$ListSpValide[i]
  print(SpTarget)
  Datei=DateData$PicSp[i]
  
  print(paste(i,SpTarget))
  #LimitF="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
  GI=GIsave
  DataSpi=subset(GI,GI$species==SpTarget)
  Phylumi=DataSpi$phylum[1]
  print(Phylumi)
  GI=subset(GI,GI$phylum==Phylumi)
  GI=subset(GI,GI$yday>Datei-30)
  GI=subset(GI,GI$yday<Datei+30)
  GI$presence=as.numeric(GI$species==SpTarget)
  summary(GI$presence)
  test=subset(GI,GI$countryCode=="IQ")
  DataMod=unique(GI,by=c("decimalLongitude","decimalLatitude"
                         ,"presence"))
  DataMod$precision=ceiling(log(DataMod$coordinateUncertaintyInMeters,10))
  DataMod$precision=pmin(4,DataMod$precision)
  DataMod$precision=pmax(1,DataMod$precision)
  DataMod$precision=ifelse(is.na(DataMod$precision),5,DataMod$precision)
  table(DataMod$precision)
  test=subset(DataMod,DataMod$countryCode=="IQ")
  
  #AggPrec1=aggregate(DataMod$phylum,DataMod$precision)
  
  #DataMod=merge(DataMod,GI,by=c("decimalLongitude","decimalLatitude"))
  summary(DataMod$presence)
  if(max(DataMod$presence)==1)
  {
    #add several rotated coordinates
    CoordDS=as.matrix(cbind(as.data.frame(DataMod$decimalLongitude)
                            ,as.data.frame(DataMod$decimalLatitude)))
    for (a in 0:(Ncoord-1))
    {
      Coordi=Rotation(CoordDS,angle=pi*a/Ncoord)
      #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
      #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
      DataMod=cbind(DataMod,Coordi[,1])
      names(DataMod)[ncol(DataMod)]=paste0("SpCoord",a)
    }
    
    testPred=(substr(names(DataMod),1,2)=="Sp")
    Prednames=names(DataMod)[testPred]
    Predictors=DataMod[,..Prednames]
    Predictors[is.na(Predictors)]=0
    
    testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
    print(summary(testNA))
    print(nrow(DataMod))
    
    #Npos=sum(DataMod$presence)
    #DownSample=max(Npos,1000)
    Strata=paste(DataMod$zone,Phylumi,DataMod$countryCode
                 ,DataMod$precision,DataMod$presence)
    table(Strata)
    NumSample=aggregate(DataMod$presence,by=list(Strata),length)
    # ListStrata=colnames(table(DataMod$presence,Strata))
    # ListStrata=subset(ListStrata,substr(ListStrata,nchar(ListStrata)
    #                                       ,nchar(ListStrata))=="0")
    test117=match(NumSample$Group.1,SummStrata)
    MaxStrata=Summaries$x[test117]
    MaxStrata[is.na(MaxStrata)]=10000
    DownSample=pmin(NumSample$x,MaxStrata)
    DownSample2=pmax(round(DownSample/2),1)
    
    #Sys.time()
    #ModRFNW=randomForest(x=Predictors,y=as.factor(DataMod$presence)
    #                    ,replace=T
    #                   ,importance=T
    #                  ,ntree=500) #0.1 sec / tree
    #Sys.time()
    
    #Sys.time()
    #ModRF=randomForest(x=Predictors,y=as.factor(DataMod$presence)
    #                  ,replace=T
    #                 ,importance=T
    #                ,sampsize=DownSample
    #               ,ntree=500) #0.1 sec / tree
    if(!is.na(sd(DataMod$presence))){
      if(sd(DataMod$presence)>0)
      {
        Sys.time()
        ModRF_morebootstrap=randomForest(x=Predictors,y=as.factor(DataMod$presence)
                                         ,replace=T
                                         ,importance=F
                                         ,strata=as.factor(Strata)
                                         ,sampsize=DownSample
                                         ,ntree=100) #0.1 sec / tree
        Sys.time()
        
        
        #varImpPlot(ModRF_morebootstrap,cex=0.5,main=paste("Presence",SpTarget))
        #print(paste("TxErreur: ",ModRFNW$confusion[2,3]))
        #varImpPlot(ModRF,cex=0.5,main=paste("Presence",SpTarget))
        #print(paste("TxErreur: ",ModRF$confusion[2,3]))
        #ModRF$confusion
        # varImpPlot(ModRF_morebootstrap,cex=0.5,main=paste("Presence",SpTarget))
        # print(paste("TxErreur: ",ModRF_morebootstrap$confusion[2,3]))
        # ModRF_morebootstrap$confusion
        # 
        #coordinates(DataMod) <- c("Group.1", "Group.2")
        #proj4string(DataMod) <- CRS("+init=epsg:4326") # WGS 84
        #DataMod$pred=ModRF$predicted
        #print(spplot(DataMod,zcol="pred",main=SpTarget))  
        
        
        #save (ModRFNW,file=paste0("./VigieChiro/ModPred/ModRFPresence_",SpTarget,"_"
        #                         ,GroupName,"_"
        #                        ,"Raw.learner")) 
        #save (ModRF,file=paste0("./VigieChiro/ModPred/ModRFPresence_",SpTarget,"_"
        #                       ,GroupName,"_"
        #                      ,"DSnorm.learner")) 
        save (ModRF_morebootstrap,file=paste0(DirOut,"/ModRFPresence_"
                                              ,SpTarget,"_"
                                              ,Phylumi,"_"
                                              ,"DS1.learner")) 
      }
    }
  }
  
  
}
