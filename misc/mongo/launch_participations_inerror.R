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
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)



#sites = mongo(collection="sites", db="vigiechiro", url=connection_string)

Sys.time()
alldatapart<-participations$find(fields='{}')
Sys.time()

table(alldatapart$traitement$etat)
datapartE=subset(alldatapart,alldatapart$traitement$etat=="ERREUR")


table(datapartE$observateur)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
#FileBMRE="C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table_bis_corrected.csv"
#fichiers= mongo(collection="fichiers", db="vigiechiro", url=connection_string)

#test=participations$export()


#filesel=fichiers$find(query='{"participation":{"$oid":"630cdd61cf100000e7007c9b"}}')

datajobs=queuer_jobs$find()
Sys.time()

dataT=subset(datajobs,datajobs$name=="process_participation")

ListPartAll=vector()
for (h in 1:nrow(dataT))
{
  if(h%%10000==1){print(paste(h,Sys.time()))}
  Parth=dataT$args[h][[1]]
  Parth=Parth[1]
  if(length(Parth)>1){stop("duplicate")}
  if(length(Parth)==0){Parth="missing"}
  ListPartAll=c(ListPartAll,Parth)
  
}
head(ListPartAll)
ListPartAll=unlist(ListPartAll)


#alldatapart<-participations$find(fields='{}')
#Sys.time()
#alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
#Sys.time() #~1sec / 1e3 sites

#DataBMRE=fread(FileBMRE)
#ListPart=DataBMRE$idparticipation
#ListPart="630cdd61cf100000e7007c9b"
table(datajobs$status)

dataError=subset(datajobs,datajobs$status=="ERROR")
dataError=subset(dataError,dataError$name=="process_participation")

ListPart=vector()
for (h in 1:nrow(dataError))
{
  Parth=dataError$args[h][[1]]
  Parth=Parth[1]
  if(length(Parth)>1){stop("duplicate")}
  ListPart=c(ListPart,Parth)
  
}
head(ListPart)
ListPart=unlist(ListPart)

table(dataError$name)

#PartRelance=vector()
for (i in 2:nrow(datapartE)){
  #ListPart=
  dataParti=subset(dataT,ListPartAll==datapartE$'_id'[i])
  dataPartiP=datapartE[i,]
  jobi=dataParti[1,]
  
  
        #stop()
        print(datapartE$'_id'[i])
        #PartRelance=c(PartRelance,ListPart[i])
        jobi$args[[1]]=ListPart[i]  
        jobi$kwargs$notify_mail=""
        jobi$kwargs$notify_msg=""
        jobi$kwargs$publique=TRUE
        jobi$status="READY"
        jobi$submitted=Sys.time()
        jobi$name="process_participation"
        
        queuer_jobs$insert(jobi)
        
        print(paste0("Insertion realisee dans queuer_jobs pour la participation ",datapartE$'_id'[i]))
      }

