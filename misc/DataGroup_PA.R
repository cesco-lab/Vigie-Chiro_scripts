library(data.table)
library(sp)
library(raster)
GroupName="POACEAE"
SpTarget="Phyllostachys nigra"
LimitF="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"

ListGroup=list.files("./VigieChiro/gbifData/DataGroup",pattern=GroupName
                     ,full.names=T)

my.data=list()
for (i in 1:length(ListGroup))
{
  my.data[[i]]=fread(ListGroup[i])
}

DataGroup=rbindlist(my.data,fill=T,use.names=T)





Limit=shapefile(LimitF)
DataSpatial=subset(DataGroup,!(is.na(DataGroup$decimalLongitude)))
coordinates(DataSpatial)=c("decimalLongitude","decimalLatitude")
proj4string(DataSpatial)=CRS("+init=epsg:4326")

LimitWGS84=spTransform(Limit,CRS(proj4string(DataSpatial)))

DataLimit=intersect(DataSpatial,LimitWGS84)
DataLimit$presence=as.numeric(DataLimit$name==SpTarget)
DataLimit$coordinateUncertaintyInMeters[is.na(DataLimit$coordinateUncertaintyInMeters)]=5000
Weight=pmin(1,50/DataLimit$coordinateUncertaintyInMeters)
DataLimit$Weight=Weight
DataPresence=subset(DataLimit,DataLimit$presence==1)
if(nrow(DataPresence)==0){stop("Pas de données de présence")}
DataAbsence=subset(DataLimit,DataLimit$presence==0)


DataSampl1=DataPresence[sample.int(nrow(DataPresence)
                                   ,size=min(1000,nrow(DataPresence))
                                                               ,replace=F
                                   ,prob=DataPresence$Weight),]
DataSampl2=DataAbsence[sample.int(nrow(DataAbsence)
                                   ,size=min(1000,nrow(DataAbsence))
                                   ,replace=F
                                   ,prob=DataAbsence$Weight),]

DataSampl=rbind(DataSampl1,DataSampl2)

#spplot(DataLimit,zcol="presence")

spplot(DataSampl,zcol="presence")

DataSamplDF=as.data.frame(DataSampl)

fwrite(DataSamplDF,paste0("./VigieChiro/GIS/PA/PA_",SpTarget,".csv"),sep=";")

