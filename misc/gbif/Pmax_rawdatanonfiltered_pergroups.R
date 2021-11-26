library(data.table)
library(raster)
library(rgdal)
library(readxl)

#SelPol=shapefile("./SIG/R12.shp")
ListTargets=fread("./VigieChiro/gbifData/DataSelDate/DataSelRare__260.csv")
DirDG="./VigieChiro/gbifData/DataGroup"
Suffix="FR"
OutputName="./www/Raw260FR"
Groupes=read_xlsx("GroupeSp.xlsx")


dir.create(OutputName)

LF=vector()
for (i in 1:length(Suffix))
{
  LFi=list.files(DirDG,full.names=T,pattern=paste0("_",Suffix))
  LF=c(LF,LFi)
}

testG=match(tstrsplit(basename(LF),split="_")[[2]],Groupes$Group)
GroupeO=Groupes$Discipline[testG]
print(table(GroupeO))
GroupeL=unique(GroupeO)
print(GroupeL)

for (j in 1:length(GroupeL))
{
  LFj=subset(LF,GroupeO==GroupeL[j])
  print(paste(Sys.time(),GroupeL[j]))
  LFjk=list()
  for (k in 1:length(LFj))
  {
    LFjk[[k]]=fread(LFj[k])
    print(names(LFjk[[k]]))
    #  NamesToSelect=subset(names(LFjk),(names(LFjk) %in% c("decimalLongitude","decimalLatitude","coordinateUncertaintyInMeters"
    #                                                      ,"year","month","day","name","yday")))
    #print(NamesToSelect)                                    
    print(dim(LFjk[[k]]))
    LFjk[[k]]$eventDate=NULL
    #LFjk[[k]]=subset(LFjk[[k]],select=NamesToSelect)
    print(dim(LFjk[[k]]))
  }
  #DataF=fread(LF[j])
  DataF=rbindlist(LFjk,use.names=T)
  print(nrow(DataF))
  DataFj=subset(DataF,DataF$name %in% ListTargets$ListSpValide)
  print(table(DataFj$name))
  
  
  if(nrow(DataFj)>0){
    if(!("coordinateUncertaintyInMeters" %in% names(DataFj))){
      DataFj$coordinateUncertaintyInMeters=NA
    }
    
    DataFj=subset(DataFj,!is.na(DataFj$decimalLongitude))
    DataFj=subset(DataFj,!is.na(DataFj$decimalLatitude))
    print(nrow(DataFj))
    coordinates(DataFj)=c("decimalLongitude","decimalLatitude")
    proj4string(DataFj) <- CRS("+init=epsg:4326") # WGS 84
    
    DataPol=DataFj
    #DataPol=raster::intersect(DataFj,SelPol)
    if(nrow(DataPol)>0){
      
      PPdf=as.data.frame(DataPol)
      #EspNew=unique(PPdf$name)
      
      
      #PMaxtot=vector()
      #for (k in 1:length(EspNew))
      #{
      PP10=subset(DataPol,DataPol$coordinateUncertaintyInMeters<10)
      #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
      if(nrow(PP10)>0){
        
        writeOGR(PP10, dsn=paste0(OutputName,"/Raw",GroupeL[j],"10_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
        print(paste0(OutputName,"/Raw",GroupeL[j],"10_RawData.geojson"))
      }
      PP100=subset(DataPol,(DataPol$coordinateUncertaintyInMeters<100)&(DataPol$coordinateUncertaintyInMeters>=10))
      #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
      if(nrow(PP100)>0){
        
        writeOGR(PP100, dsn=paste0(OutputName,"/Raw",GroupeL[j],"100_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
      }
      PP1000=subset(DataPol,(DataPol$coordinateUncertaintyInMeters<1000)&(DataPol$coordinateUncertaintyInMeters>=100))
      #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
      if(nrow(PP1000)>0){
        
        writeOGR(PP1000, dsn=paste0(OutputName,"/Raw",GroupeL[j],"1000_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
      }
      PPNA=subset(DataPol,is.na(DataPol$coordinateUncertaintyInMeters))
      if(nrow(PPNA)>0){
        
        writeOGR(PPNA, dsn=paste0(OutputName,"/Raw",GroupeL[j],"NA_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
        print(paste0(OutputName,"/Raw",GroupeL[j],"NA_RawData.geojson"))
      }
      
      
      
    }
    
    
  }
}
