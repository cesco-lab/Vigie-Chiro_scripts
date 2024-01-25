library(mongolite)
library(data.table)
library(beepr)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(uuid)
library(jsonlite)
library(lubridate)

connection_string = "mongodb://vigiechiro:4Zp9nh8d2YSVt5@ccmg01.in2p3.fr:27030,ccmg02.in2p3.fr:27030,ccmg03.in2p3.fr:27030/vigiechiro?replicaSet=rs0&readPreference=secondaryPreferred&socketTimeoutMS=6000000" #site de prod
#connection_string = "mongodb://vigiechiro:kafc2xdr8vZdBpuV@ccdbmgtstno01.in2p3.fr:27080,ccdbmgtstno03.in2p3.fr:27080,ccdbmgtstno02.in2p3.fr:27080/vigiechiro?replicaSet=rs0&authSource=vigiechiro&socketTimeoutMS=6000000" #site de test



sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
FileBMRE="C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table_bis_created.csv"
#test=participations$export()


Sys.time()
alldatapart<-participations$find(fields='{}')
Sys.time()
#alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
#Sys.time() #~1sec / 1e3 sites

DataBMRE=fread(FileBMRE)

ParticipationCorrect=vector()
for (i in 1:nrow(DataBMRE)){
  parti1=subset(alldatapart,alldatapart$site==DataBMRE$idsite[i])
  parti2=subset(parti1,parti1$point==DataBMRE$Point[i])
  dateB=dmy(DataBMRE$StartDate)
  dateP=as.Date(parti2$date_debut)
  parti3=subset(parti2,dateP==dateB[i])
print(  parti3$'_id')
  ParticipationCorrect=c(ParticipationCorrect,parti3$'_id')
}

DataBMRE$idparticipation=ParticipationCorrect

fwrite(DataBMRE,gsub("_created","_corrected",FileBMRE),sep=";")
