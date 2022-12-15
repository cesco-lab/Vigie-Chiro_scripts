library(mongolite)
library(data.table)
library(beepr)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(uuid)
library(jsonlite)

mongo=fread("mongos.txt",sep="$",h=F)
test=F
ToSuppress=data.frame(p="58fe0beabc0b3f000e89f628")
ToSuppress=fread("ToSuppress221201.csv",h=F)


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
#listpart=fread("ccsps220906.csv")
#test=participations$export()

Sys.time()
alldatapart<-participations$find(fields='{}')
Sys.time()

names(ToSuppress)="V1"
LPS=ToSuppress$V1
  
for (i in 1:length(LPS)){
#test=  participations$find(query=paste0('{"_id":{"$oid":"',LPS[i],'"}}'))
  print(LPS[i])
participations$remove(query=paste0('{"_id":{"$oid":"',LPS[i],'"}}'))  
}
