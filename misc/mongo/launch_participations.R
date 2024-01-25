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



queuer_jobs = mongo(collection="queuer_jobs", db="vigiechiro", url=connection_string)

#sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
#participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
FileBMRE="C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table_bis_corrected.csv"
#fichiers= mongo(collection="fichiers", db="vigiechiro", url=connection_string)

#test=participations$export()


#filesel=fichiers$find(query='{"participation":{"$oid":"630cdd61cf100000e7007c9b"}}')

datajobs=queuer_jobs$find()
Sys.time()
#alldatapart<-participations$find(fields='{}')
#Sys.time()
#alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
#Sys.time() #~1sec / 1e3 sites

DataBMRE=fread(FileBMRE)
ListPart=DataBMRE$idparticipation
#ListPart="630cdd61cf100000e7007c9b"


for (i in 1:length(ListPart)){
  jobi=datajobs[1,]
  
  jobi$args[[1]]=ListPart[i]  
  jobi$kwargs$notify_mail=""
  jobi$kwargs$notify_msg=""
  jobi$kwargs$publique=TRUE
  jobi$status="READY"
  jobi$submitted=Sys.time()
  jobi$name="process_participation"
  
  queuer_jobs$insert(jobi)
  
  print(paste0("Insertion realisee dans queuer_jobs pour la participation ",ListPart[i]))
        
}
