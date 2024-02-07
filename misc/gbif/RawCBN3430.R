library(data.table)
#library(raster)
#library(rgdal)
library(sf)
library(readxl)
library(foreign)

#SelPol=shapefile("./SIG/R12.shp")
#ListTargets=fread("mnt/DataSelDate/DataSelRare__70.csv")
#ListTargets=fread("C:/Users/ybas/Documents/VigieChiro/gbifData/DataSelDate/DataSel__155.csv")
DirDG="C:/Users/ybas/Documents/natura/siflore/HERAULT/DBF"
Data30=fread("C:/Users/ybas/Documents/SIG/cbn30.csv")
#DataF=fread("C:/Users/ybas/Documents/natura/siflore/HERAULT/DBF/all.csv",sep="µ")
#Suffix="FR"
OutputName="C:/Users/ybas/Documents/www/Raw67"
#Groupes=read_xlsx("GroupeSp.xlsx")
SpeciesAll=fread("SpeciesAll.csv",sep=";")
Date=67
SpSel=c("Aristolochia rotunda","Aristolochia pistolochia")
SpSel=NA
dir.create(OutputName)




# ListTargets=subset(ListTargets
#                    ,!(ListTargets$ListSpValide %in% SpeciesAll$Scientific.name))



LF=list.files(DirDG,pattern=".dbf$",full.names=T)

LFd=list()
for (i in 1:length(LF)){
  LFd[[i]]=read.dbf(LF[i])
  
}
DataF=rbindlist(LFd)
test=match(names(DataF),names(Data30))
DataF=rbindlist(list(DataF,Data30),use.names=T,fill=T)



table(DataF$precision_)
DataF=subset(DataF,grepl("Pointage",DataF$precision_))

print(nrow(DataF))

SpI=tstrsplit(DataF$nom_scient,split=" ")
DataF$name=paste(SpI[[1]],SpI[[2]])

DataSp=subset(DataF,DataF$name=="Aristolochia rotunda")

#table(DataF$jour=="00",DataF$mois=="00")
DataF=subset(DataF,DataF$jour!="00")
DataF=subset(DataF,DataF$jour!="0")
DataF=subset(DataF,DataF$mois!="00")
DataF=subset(DataF,DataF$mois!="0")
hist(as.numeric(as.character(DataF$jour)))
hist(as.numeric(as.character(DataF$mois)))

DataF$yday=as.numeric(as.character(DataF$jour))+(as.numeric(as.character(DataF$mois))-1)*30
summary(DataF$yday)
hist(DataF$yday)
# DataFj=subset(DataF,DataF$name %in% ListTargets$ListSpValide)
# print(table(DataFj$name))
# 
# DataFj=as.data.frame(DataFj)
# 
# DataFj
# 
# DataFj=subset(DataFj,select=c("name","longitude_","latitude_w"))


if(!is.na(SpSel)){
  DataFj=subset(DataF
                ,DataF$name %in% SpSel)
  
  
  
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
        print(paste0(OutputName,"/",tstrsplit(SpSel[1],split=" ")[[1]],"_CBN_RawData.geojson"))
      }
      
    }
    
    
  }
  
  
}else{
  DataF=subset(DataF
               ,!(DataF$name %in% SpeciesAll$Scientific.name))
  
  
  
  DataFj=subset(DataF,DataF$yday>Date-15)
  DataFj=subset(DataFj,DataFj$yday<Date+15)
  
  
  DataFj=subset(DataFj,select=c("name","longitude_","latitude_w"))
  
  
  if(nrow(DataFj)>0){
    DataFj=subset(DataFj,!is.na(DataFj$longitude_))
    DataFj=subset(DataFj,!is.na(DataFj$latitude_w))
    print(nrow(DataFj))
    # Create a Simple Features (sf) object
    DataFj_sf <- st_as_sf(DataFj, coords = c("longitude_", "latitude_w"), crs = 2154)
    
    # Transform the coordinate reference system (CRS) to EPSG:4326
    DataPol <- st_transform(DataFj_sf, crs = 4326)
    # coordinates(DataFj)=c("longitude_","latitude_w")
    # proj4string(DataFj) <- CRS("+init=epsg:2154") # L93
    # 
    # 
    # DataPol=spTransform(DataFj,CRS("+init=epsg:4326"))
    # #DataPol=raster::intersect(DataFj,SelPol)
    if(nrow(DataPol)>0){
      
      PPdf=as.data.frame(DataPol)
      #EspNew=unique(PPdf$name)
      
      
      #PMaxtot=vector()
      #for (k in 1:length(EspNew))
      #{
      PP10=DataPol
      #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
      if(nrow(PP10)>0){
        st_write(PP10, dsn = paste0(OutputName, "/CBN_RawData.geojson"), driver = "GeoJSON", delete_dsn = TRUE)
        #writeOGR(PP10, dsn=paste0(OutputName,"/CBN_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
        print(paste0(OutputName,"/CBN_RawData.geojson"))
      }
      
    }
    
    
  }
  
}

head(table(PPdf$name)[order(table(PPdf$name),decreasing=T)])
