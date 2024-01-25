library(data.table)
library(raster)
library(rgdal)
library(readxl)
library(foreign)

#SelPol=shapefile("./SIG/R12.shp")
#ListTargets=fread("mnt/DataSelDate/DataSelRare__70.csv")
ListTargets=fread("C:/Users/yvesb/Documents/VigieChiro/gbifData/DataSelDate/DataSel__205.csv")
DirDG="C:/Users/yvesb/Documents/natura/siflore/HERAULT/DBF"
#DataF=fread("C:/Users/yvesb/Documents/natura/siflore/HERAULT/DBF/all.csv",sep="µ")
#Suffix="FR"
OutputName="C:/Users/yvesb/Documents/www/Raw205FR"
#Groupes=read_xlsx("GroupeSp.xlsx")
SpeciesAll=fread("SpeciesAll.csv",sep=";")


dir.create(OutputName)

ListTargets=subset(ListTargets
                   ,!(ListTargets$ListSpValide %in% SpeciesAll$Scientific.name))



LF=list.files(DirDG,pattern=".dbf$",full.names=T)

LFd=list()
for (i in 1:length(LF)){
  LFd[[i]]=read.dbf(LF[i])
  
}
DataF=rbindlist(LFd)

print(nrow(DataF))

SpI=tstrsplit(DataF$nom_scient,split=" ")
DataF$name=paste(SpI[[1]],SpI[[2]])
DataFj=subset(DataF,DataF$name %in% ListTargets$ListSpValide)
print(table(DataFj$name))

DataFj=as.data.frame(DataFj)
DataFj=subset(DataFj,select=c("name","longitude_","latitude_w"))

if(nrow(DataFj)>0){
  DataFj=subset(DataFj,!is.na(DataFj$longitude_))
  DataFj=subset(DataFj,!is.na(DataFj$latitude_w))
  print(nrow(DataFj))
  coordinates(DataFj)=c("longitude_","latitude_w")
  proj4string(DataFj) <- CRS("+init=epsg:2154") # L93
  
  
  DataPol=spTransform(DataFj,CRS("+init=epsg:4326"))
  #DataPol=raster::intersect(DataFj,SelPol)
  if(nrow(DataPol)>0){
    
    PPdf=as.data.frame(DataPol)
    #EspNew=unique(PPdf$name)
    
    
    #PMaxtot=vector()
    #for (k in 1:length(EspNew))
    #{
    PP10=DataPol
    #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
    if(nrow(PP10)>0){
      
      writeOGR(PP10, dsn=paste0(OutputName,"/CBN_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
      print(paste0(OutputName,"/CBN_RawData.geojson"))
    }
    
  }
  
  
}

