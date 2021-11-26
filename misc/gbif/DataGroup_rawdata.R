library(data.table)
library(raster)
library(rgdal)

#SelPol=shapefile("./SIG/R12.shp")
ListTargets=fread("./VigieChiro/gbifData/DataSelDate/DataSel__230.csv")
DirDG="./VigieChiro/gbifData/DataGroup/HR_SI"
Suffix="SI"
OutputName="./www/Raw230SI"
SpeciesAll=fread("SpeciesAll.csv",sep=";")
DataSelDir="./VigieChiro/gbifData/DataSelDate"


dir.create(OutputName)
dir.create(paste0(OutputName,"/peak_alr/"))
dir.create(paste0(OutputName,"/peak_non/"))
dir.create(paste0(OutputName,"/hp_alr/"))
dir.create(paste0(OutputName,"/hp_non/"))
dir.create(paste0(OutputName,"/unknown/"))

FDS=list.files(DataSelDir,pattern="DataSel__",full.names=T)
FDS0=subset(FDS,grepl("0.csv",FDS))

data0=list()
for (h in 1:length(FDS0))
{
  data0[[h]]=fread(FDS0[h])
}
data0all=rbindlist(data0)

dir.create(OutputName)

LF=vector()
for (i in 1:length(Suffix))
{
  LFi=list.files(DirDG,full.names=T,pattern=paste0("_",Suffix))
  LF=c(LF,LFi)
}


for (j in 1:length(LF))
{
  print(j)
  DataF=fread(LF[j])
  DataFj0=subset(DataF,DataF$name %in% ListTargets$ListSpValide)
  table(DataFj0$name)
  
  #cas1 : coche
  DataFj=subset(DataFj0,!(DataFj0$name %in% SpeciesAll$Scientific.name))
  print(table(DataFj$name))
  
  DataFj=subset(DataFj,!is.na(DataFj$decimalLongitude))
  DataFj=subset(DataFj,!is.na(DataFj$decimalLatitude))
  if(nrow(DataFj)>0){
    
    coordinates(DataFj)=c("decimalLongitude","decimalLatitude")
    proj4string(DataFj) <- CRS("+init=epsg:4326") # WGS 84
    
    DataPol=DataFj
    #DataPol=raster::intersect(DataFj,SelPol)
    if(nrow(DataPol)>0){
      
      PPdf=as.data.frame(DataPol)
      EspNew=unique(PPdf$name)
      
      PMaxtot=vector()
      for (k in 1:length(EspNew))
      {
        PPk=subset(DataPol,DataPol$name==EspNew[k])
        if("coordinateUncertaintyInMeters" %in% names(PPdf)){
          PPk=PPk[order(PPk$coordinateUncertaintyInMeters),]
        }
        test=duplicated(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        PPk=subset(PPk,!test)
        #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        writeOGR(PPk, dsn=paste0(OutputName,"/peak_non/",gsub(" ","_",EspNew[i]),"_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
      }
    }
  }
  #cas2 : already
  DataFj=subset(DataFj0,(DataFj0$name %in% SpeciesAll$Scientific.name))
  table(DataFj$name)
  
  DataFj=subset(DataFj,!is.na(DataFj$decimalLongitude))
  DataFj=subset(DataFj,!is.na(DataFj$decimalLatitude))
  if(nrow(DataFj)>0){
    
    coordinates(DataFj)=c("decimalLongitude","decimalLatitude")
    proj4string(DataFj) <- CRS("+init=epsg:4326") # WGS 84
    
    DataPol=DataFj
    #DataPol=raster::intersect(DataFj,SelPol)
    if(nrow(DataPol)>0){
      
      PPdf=as.data.frame(DataPol)
      EspNew=unique(PPdf$name)
      
      PMaxtot=vector()
      for (k in 1:length(EspNew))
      {
        PPk=subset(DataPol,DataPol$name==EspNew[k])
        if("coordinateUncertaintyInMeters" %in% names(PPdf)){
          PPk=PPk[order(PPk$coordinateUncertaintyInMeters),]
        }
        test=duplicated(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        PPk=subset(PPk,!test)
        #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        writeOGR(PPk, dsn=paste0(OutputName,"/peak_alr/",gsub(" ","_",EspNew[i]),"_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
      }
    }
    
  }
  #cas3 : unknown
  
  DataFj=subset(DataF,!(DataF$name %in% data0all$ListSpValide))
  print(table(DataFj$name))
  
  DataFj=subset(DataFj,!is.na(DataFj$decimalLongitude))
  DataFj=subset(DataFj,!is.na(DataFj$decimalLatitude))
  if(nrow(DataFj)>0){
    
    coordinates(DataFj)=c("decimalLongitude","decimalLatitude")
    proj4string(DataFj) <- CRS("+init=epsg:4326") # WGS 84
    
    DataPol=DataFj
    #DataPol=raster::intersect(DataFj,SelPol)
    if(nrow(DataPol)>0){
      
      PPdf=as.data.frame(DataPol)
      EspNew=unique(PPdf$name)
      
      PMaxtot=vector()
      for (k in 1:length(EspNew))
      {
        PPk=subset(DataPol,DataPol$name==EspNew[k])
        if("coordinateUncertaintyInMeters" %in% names(PPdf)){
          PPk=PPk[order(PPk$coordinateUncertaintyInMeters),]
        }
        test=duplicated(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        PPk=subset(PPk,!test)
        #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        writeOGR(PPk, dsn=paste0(OutputName,"/unknown/",gsub(" ","_",EspNew[i]),"_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
      }
    }
    
  }
  
  #cas4 : hors pic coche
  DataFj0=subset(DataF,!(DataF$name %in% ListTargets$ListSpValide))
  DataFj=subset(DataFj0,(DataFj0$name %in% SpeciesAll$Scientific.name))
  table(DataFj$name)
  
  DataFj=subset(DataFj,!is.na(DataFj$decimalLongitude))
  DataFj=subset(DataFj,!is.na(DataFj$decimalLatitude))
  if(nrow(DataFj)>0){
    
    coordinates(DataFj)=c("decimalLongitude","decimalLatitude")
    proj4string(DataFj) <- CRS("+init=epsg:4326") # WGS 84
    
    DataPol=DataFj
    #DataPol=raster::intersect(DataFj,SelPol)
    if(nrow(DataPol)>0){
      
      PPdf=as.data.frame(DataPol)
      EspNew=unique(PPdf$name)
      
      PMaxtot=vector()
      for (k in 1:length(EspNew))
      {
        PPk=subset(DataPol,DataPol$name==EspNew[k])
        if("coordinateUncertaintyInMeters" %in% names(PPdf)){
          PPk=PPk[order(PPk$coordinateUncertaintyInMeters),]
        }
        test=duplicated(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        PPk=subset(PPk,!test)
        #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        writeOGR(PPk, dsn=paste0(OutputName,"/hp_non/",gsub(" ","_",EspNew[i]),"_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
      }
    }
    
  }
  
  #cas 5 : hors pic alre
  DataFj0=subset(DataF,!(DataF$name %in% ListTargets$ListSpValide))
  DataFj=subset(DataFj0,!(DataFj0$name %in% SpeciesAll$Scientific.name))
  table(DataFj$name)
  
  DataFj=subset(DataFj,!is.na(DataFj$decimalLongitude))
  DataFj=subset(DataFj,!is.na(DataFj$decimalLatitude))
  if(nrow(DataFj)>0){
    
    coordinates(DataFj)=c("decimalLongitude","decimalLatitude")
    proj4string(DataFj) <- CRS("+init=epsg:4326") # WGS 84
    
    DataPol=DataFj
    #DataPol=raster::intersect(DataFj,SelPol)
    if(nrow(DataPol)>0){
      
      PPdf=as.data.frame(DataPol)
      EspNew=unique(PPdf$name)
      
      PMaxtot=vector()
      for (k in 1:length(EspNew))
      {
        PPk=subset(DataPol,DataPol$name==EspNew[k])
        if("coordinateUncertaintyInMeters" %in% names(PPdf)){
          PPk=PPk[order(PPk$coordinateUncertaintyInMeters),]
        }
        test=duplicated(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        PPk=subset(PPk,!test)
        #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
        writeOGR(PPk, dsn=paste0(OutputName,"/hp_alr/",gsub(" ","_",EspNew[i]),"_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
      }
    }
    
  }
  
  
  
  
  
}

