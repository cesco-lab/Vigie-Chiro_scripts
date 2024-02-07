library(data.table)
library(sf)
library(raster)
library(sp)

DataGCLR=fread("C:/Users/ybas/Downloads/_SELECT_s_id_permanent_as_id_permanent_sinp_s_id_obs_as_id_obs_m_202402012219.csv")
FiltreCommune=F
Commune=c("Counozouls")
FileOutput="C:/Users/ybas/Documents/chiros/GCLR/Exports/DataTA.csv"
FiltreShape=T
Shape=st_read("C:/Users/ybas/Downloads/SecteurReferentsProspections.shp.shp")
NomShape="Thierry Alignan"

DataFiltered=DataGCLR

if(FiltreCommune){
DataFiltered=subset(DataFiltered,DataFiltered$nom_commune %in% Commune)
}

if(FiltreShape){
ShapeF=subset(Shape,Shape$Nom %in% NomShape)
DataFiltered=subset(DataFiltered,!is.na(DataFiltered$longitude))
coordinates(DataFiltered)=c("longitude","latitude")    
proj4string(DataFiltered) <- CRS("+init=epsg:4326") # WGS 84
DataFiltered=st_as_sf(DataFiltered)
ShapeF=st_transform(ShapeF,crs(DataFiltered))
DataFiltered=st_intersection(DataFiltered,ShapeF)

}


fwrite(DataFiltered,FileOutput,sep=";")
  
  
