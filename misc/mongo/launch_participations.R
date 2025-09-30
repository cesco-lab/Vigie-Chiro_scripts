library(mongolite)
library(data.table)
library(beepr)
library(raster)
library(uuid)
library(jsonlite)
library(lubridate)


mongo=fread("mongos.txt",sep="$",h=F)
test=F #T si base de test, F si baase de prod
FileBMRE="Relance241011.csv"
TempsPause=60

if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}



queuer_jobs = mongo(collection="queuer_jobs", db="vigiechiro", url=connection_string)

#sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
#participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)

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

ListPart=subset(ListPart,ListPart!="")

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
  Sys.sleep(TempsPause)      
}
