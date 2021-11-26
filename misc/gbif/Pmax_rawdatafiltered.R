library(data.table)
library(raster)
library(rgdal)
library(readxl)

SelPol=shapefile("./SIG/R12.shp")
ListTargets=fread("./VigieChiro/gbifData/DataSelDate/DataSelRare__240.csv")
DirDG="./VigieChiro/gbifData/DataGroup"
Suffix="FR"
OutputName="./www/Raw240FR"
Groupes=read_xlsx("GroupeSp.xlsx")

dir.create(OutputName)

LF=vector()
for (i in 1:length(Suffix))
{
  LFi=list.files(DirDG,full.names=T,pattern=paste0("_",Suffix))
  LF=c(LF,LFi)
  }


for (j in 1:length(LF))
{
  DataF=fread(LF[j])
  DataFj=subset(DataF,DataF$name %in% ListTargets$ListSpValide)
table(DataFj$name)

if(nrow(DataFj)>0){
DataFj=subset(DataFj,!is.na(DataFj$decimalLongitude))
DataFj=subset(DataFj,!is.na(DataFj$decimalLatitude))
coordinates(DataFj)=c("decimalLongitude","decimalLatitude")
proj4string(DataFj) <- CRS("+init=epsg:4326") # WGS 84

DataPol=raster::intersect(DataFj,SelPol)
if(nrow(DataPol)>0){

PPdf=as.data.frame(DataPol)
EspNew=unique(PPdf$name)

PMaxtot=vector()
for (k in 1:length(EspNew))
{
  PPk=subset(DataPol,DataPol$name==EspNew[k])
  PPk=PPk[order(PPk$coordinateUncertaintyInMeters),]
  test=duplicated(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
  PPk=subset(PPk,!test)
  #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
  writeOGR(PPk, dsn=paste0(OutputName,"/",gsub(" ","_",EspNew[i]),"_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
  }


}
}
}
