library(mongolite)
library(data.table)
library(beepr)
library(uuid)
library(jsonlite)
library(lubridate)

connection_string = "mongodb://vigiechiro:4Zp9nh8d2YSVt5@ccmg01.in2p3.fr:27030,ccmg02.in2p3.fr:27030,ccmg03.in2p3.fr:27030/vigiechiro?replicaSet=rs0&readPreference=secondaryPreferred&socketTimeoutMS=6000000" #site de prod
#connection_string = "mongodb://vigiechiro:kafc2xdr8vZdBpuV@ccdbmgtstno01.in2p3.fr:27080,ccdbmgtstno03.in2p3.fr:27080,ccdbmgtstno02.in2p3.fr:27080/vigiechiro?replicaSet=rs0&authSource=vigiechiro&socketTimeoutMS=6000000" #site de test


#queuer_jobs = mongo(collection="queuer_jobs", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
NbJourEC=4


#sites = mongo(collection="sites", db="vigiechiro", url=connection_string)

Sys.time()
alldatapart<-participations$find(fields='{}')
Sys.time()
alldataobs<-users$find(fields='{}')
Sys.time()

table(alldatapart$traitement$etat)
datapartE=subset(alldatapart,alldatapart$traitement$etat=="ERREUR")


table(datapartE$observateur)

test=match(datapartE$observateur,alldataobs$'_id')

table(alldataobs$pseudo[test])

datapartEC=subset(alldatapart,alldatapart$traitement$etat=="EN_COURS")

datapartECold=subset(datapartEC,datapartEC$traitement$date_debut<Sys.time()-NbJourEC*24*3600)

test=match(datapartECold$observateur,alldataobs$'_id')

table(alldataobs$pseudo[test])

datapartECnew=subset(datapartEC,datapartEC$traitement$date_debut>=Sys.time()-NbJourEC*24*3600)

test=match(datapartECnew$observateur,alldataobs$'_id')

datapartECnew$traitement$date_debut

table(alldataobs$pseudo[test])

ListPartECnew=cbind(alldataobs$pseudo[test],as.Date(datapartECnew$traitement$date_debut))

fwrite(ListPartECnew,paste0("ListPartECnew",Sys.Date(),".csv"),sep=";")


#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
#FileBMRE="C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table_bis_corrected.csv"
#fichiers= mongo(collection="fichiers", db="vigiechiro", url=connection_string)

ListPartE=datapartE$'_id'

#ListPartE=as.data.frame(ListPartE)

#fwrite(ListPartE,paste0("ListPartE",Sys.Date(),".csv"),sep=";")

ListPartECold=datapartECold$'_id'

#ListPartECold=as.data.frame(ListPartECold)

#fwrite(ListPartECold,paste0("ListPartECold",Sys.Date(),".csv"),sep=";")


ListPartEall=c(ListPartE,ListPartECold)

ListPartEall=as.data.frame(ListPartEall)

fwrite(ListPartEall,paste0("ListPartEall",Sys.Date(),".csv"),sep=";")


datapartP=subset(alldatapart,alldatapart$traitement$etat=="PLANIFIE")

test=match(datapartP$observateur,alldataobs$'_id')

table(alldataobs$pseudo[test])






