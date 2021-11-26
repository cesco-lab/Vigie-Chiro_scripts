library(data.table)
library(stringr)
library(rgbif)
PrioTaxa=fread("DataPriorF_57.csv")
#SpeciesAll=fread("SpeciesAll.csv",sep=";")
Target=200000
Renew=F
Start="Geranium rotundifolium"

if(!Renew)
{
  Lprev=list.files(".",pattern="PrioCoord")
  Lgood=subset(Lprev,str_count(Lprev,"_")==3)
}
DataLprev=list()
for (h in 1:length(Lgood))
{
  DataLprev[[h]]=fread(Lgood[h])
}
DataPrev=rbindlist(DataLprev,use.names=T,fill=T)

if(is.na(Start)) 
{
  i=0
}else{
  i=match(Start,PrioTaxa$ListSpValide)
}
PrioCoord=data.frame()
while((nrow(PrioCoord)<Target)&(i<nrow(PrioTaxa)))
{
  i=i+1
  print(nrow(PrioCoord))
  test=name_backbone(name=PrioTaxa$ListSpValide[i],rank="SPECIES")
  Cond=(("species" %in% names(test)))
  #if(!Cond)
  #{
   # Cond=(!(test$species %in% SpeciesAll$SpeciesGBIF))
  #}
  
  if(Cond)
  {
    
    
    
    LGi=list.files("./VigieChiro/gbifData/DataGroup"
                   ,pattern=paste0("_",PrioTaxa$Group[i]),full.names=T)
    
    Grouplist=list()
    for (j in 1:length(LGi))
    {
      Grouplist[[j]]=fread(LGi[j])
    }
    Groupi=rbindlist(Grouplist,use.names=T,fill=T)
    Spi=subset(Groupi,Groupi$name==PrioTaxa$ListSpValide[i])
    Coordi=unique(Spi,by=c("decimalLongitude","decimalLatitude"))
    Coordi=subset(Coordi,!is.na(Coordi$decimalLatitude))
    Coordi=subset(Coordi,!(paste(Coordi$decimalLatitude,Coordi$decimalLongitude)
                           %in% paste(DataPrev$decimalLatitude
                                      ,DataPrev$decimalLongitude))
    )
    
    
    if(nrow(Coordi)>0){
      plot(Coordi$decimalLongitude,Coordi$decimalLatitude
           ,main=PrioTaxa$ListSpValide[i])
      PrioCoord=rbindlist(list(PrioCoord,Coordi),use.names=T,fill=T)
    }
  }
}
summary(as.factor(PrioCoord$name))
LastTaxa=gsub(" ","_",PrioTaxa$ListSpValide[i])
fwrite(PrioCoord,paste0("PrioCoord_",Sys.Date(),"_",LastTaxa,".csv"))
summary(PrioCoord$decimalLongitude)
