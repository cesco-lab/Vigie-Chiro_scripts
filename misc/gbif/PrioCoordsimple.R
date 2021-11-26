library(data.table)
library(stringr)
library(rgbif)
#PrioTaxa=fread("DataPriorF_57.csv")
#SpeciesAll=fread("SpeciesAll.csv",sep=";")
Target=200000
Renew=T
Start=NA
PrioDir="."
DGDir="./VigieChiro/gbifData/DataGroup"

LGtot=list.files(DGDir
                 ,pattern=".csv",full.names=T)

Groups=unique(tstrsplit(basename(LGtot),split="_")[[2]])

if(!Renew)
{
  Lprev=list.files(PrioDir,pattern="PrioCoord")
  Lgood=subset(Lprev,str_count(Lprev,"_")==3)
  
  DataLprev=list()
  for (h in 1:length(Lgood))
  {
    DataLprev[[h]]=fread(Lgood[h])
  }
  DataPrev=rbindlist(DataLprev,use.names=T,fill=T)
}else{
  DataPrev=data.frame()
}


if(is.na(Start)) 
{
  i=0
}else{
  i=match(Start,PrioTaxa$ListSpValide)
}
while(i<length(Groups))
{
  
  
  PrioCoord=data.frame()
  while((nrow(PrioCoord)<Target)&(i<length(Groups)))
  {
    print(paste("i",i))
    i=i+1
    print(nrow(PrioCoord))
    #if(!Cond)
    #{
    # Cond=(!(test$species %in% SpeciesAll$SpeciesGBIF))
    #}
    
    #if(Cond)
    #{
    
    
    
    LGi=list.files("./VigieChiro/gbifData/DataGroup"
                   ,pattern=paste0("_",Groups[i]),full.names=T)
    
    
    
    
    Grouplist=list()
    for (j in 1:length(LGi))
    {
      print(paste("j",j))
      Grouplist[[j]]=fread(LGi[j])
    }
    Groupi=rbindlist(Grouplist,use.names=T,fill=T)
    
    #    test=name_backbone(name=PrioTaxa$ListSpValide[i],rank="SPECIES")
    #Cond=(("species" %in% names(test)))
    
    #Spi=subset(Groupi,Groupi$name==PrioTaxa$ListSpValide[i])
    Groupi$coord=paste(Groupi$decimalLatitude,Groupi$decimalLongitude)
    Coordi=unique(Groupi,by="coord")
    Coordi=subset(Coordi,!is.na(Coordi$decimalLatitude))
    test1=paste(Coordi$decimalLatitude,Coordi$decimalLongitude)
    test2=paste(PrioCoord$decimalLatitude
                ,PrioCoord$decimalLongitude)
    print(paste(i,summary(test1 %in% test2)))
    print(summary(test1=="51.497657 5.448407"))
    #s1=subset(Coordi,Coordi$decimalLatitude=="51.497657")
    #s1=unique(s1,by=c("decimalLongitude","decimalLatitude"))
    #s1$coord=paste(s1$decimalLatitude,s1$decimalLongitude)
    #s1=unique(s1,by="coord")
    print(summary(test2=="51.497657 5.448407"))
    Coordi=subset(Coordi,!(paste(Coordi$decimalLatitude,Coordi$decimalLongitude)
                           %in% paste(PrioCoord$decimalLatitude
                                      ,PrioCoord$decimalLongitude))
    )
    test1=paste(Coordi$decimalLatitude,Coordi$decimalLongitude)
    print(summary(test1=="51.497657 5.448407"))
    
    
    
    if(nrow(Coordi)>0){
      #plot(Coordi$decimalLongitude,Coordi$decimalLatitude
       #    ,main=Groups[i])
      PrioCoord=rbindlist(list(PrioCoord,Coordi),use.names=T,fill=T)
      DataPrev=rbindlist(list(PrioCoord,DataPrev),use.names=T,fill=T)
      }
    #}
  }
  print(head(summary(as.factor(PrioCoord$name))[order(summary(as.factor(PrioCoord$name)),decreasing=T)]))
  LastTaxa=Groups[i]
  fwrite(PrioCoord,paste0(PrioDir,"/PrioCoord_",basename(DGDir),"_"
                          ,Sys.Date(),"_",LastTaxa,".csv"))
  print(summary(PrioCoord$decimalLongitude))
}

test=unique(PrioCoord,by=c("decimalLongitude","decimalLatitude"))
nrow(test)/nrow(PrioCoord)
Doub=aggregate(PrioCoord$year,by=c(list(PrioCoord$decimalLongitude)
                                   ,list(PrioCoord$decimalLatitude))
               ,length)
subset(Doub,Doub$x>1)$Group.1[1]
subset(Doub,Doub$x>1)$Group.2[1]
paste(subset(Doub,Doub$x>1)$Group.2[1],subset(Doub,Doub$x>1)$Group.1[1])
