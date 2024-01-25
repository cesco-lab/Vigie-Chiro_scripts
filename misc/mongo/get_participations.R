library(mongolite)
library(data.table)
#library(beepr)
library(raster)
#library(rgdal)
#library(maptools)
#library(rgeos)
library(uuid)
library(jsonlite)


mongo=fread("mongos.txt",sep="$",h=F)
test=F #T si base de test, F si base de prod
LPS=fread("ListPartStuck.csv")

if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
#listpart=fread("ccsps220906.csv")
#test=participations$export()
listpart=NA
users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
NbJourEC=4


Sys.time()
alldatapart<-participations$find(fields='{}') # ~1 sec / 1e3 participations
Sys.time()
alldatasites <- sites$find(fields='{}')
Sys.time() #~1sec / 1e3 sites
alldataobs <- users$find(fields='{}')
Sys.time()

if(is.na(listpart)){
  Seldatapart=alldatapart
}else{
  Seldatapart=subset(alldatapart,alldatapart$'_id' %in% listpart$participation)
}

testS=match(Seldatapart$site,alldatasites$'_id')
Seldatapart$numcarre=alldatasites$titre[testS]


summary(is.na(Seldatapart$numcarre))
testO=match(Seldatapart$observateur,alldataobs$'_id')

duree_traitement=Seldatapart$traitement$date_fin-Seldatapart$traitement$date_debut


nbData=vector()
for (i in 1:nrow(Seldatapart)){
DataC=sum(Seldatapart$bilan$chiropteres[i][[1]]$nb_contact_max)
DataO=sum(Seldatapart$bilan$orthopteres[i][[1]]$nb_contact_max)
DataA=sum(Seldatapart$bilan$autre[i][[1]]$nb_contact_max)
nbData[i]=DataC+DataO+DataA
}
summary(nbData)

sps=(substr(alldatapart$commentaire,1,1)=="#")

sps=ifelse(is.na(sps),0,sps)

p_export=data.frame(participation=Seldatapart$'_id',date=Seldatapart$'_created',idobservateur=Seldatapart$observateur
                    ,observateur=alldataobs$pseudo[testO]
                    ,duree_traitement=as.numeric(duree_traitement)
                    ,ratio_traitement=nbData/as.numeric(duree_traitement)
                    ,nb_tc=NA
                    ,nb_obs=nbData,sps=sps,dif_wav_ta=NA,pourc_dif=NA,idsite=Seldatapart$site
                    ,site=Seldatapart$numcarre,date_debut=Seldatapart$date_debut,date_fin=Seldatapart$date_fin
                    ,trait_debut=Seldatapart$traitement$date_debut,trait_fin=Seldatapart$traitement$date_fin
                    ,trait_etat=Seldatapart$traitement$etat,point=Seldatapart$point
                    ,temperature_debut=Seldatapart$meteo$temperature_debut
                    ,temperature_fin=Seldatapart$meteo$temperature_fin,couverture=Seldatapart$meteo$couverture
                    ,vent=Seldatapart$meteo$vent
                    ,detecteur_enregistreur_type=Seldatapart$configuration$detecteur_enregistreur_type
                    ,detecteur_enregistreur_numero_serie=Seldatapart$configuration$detecteur_enregistreur_numero_serie
                    ,canal_expansion_temps=Seldatapart$configuration$canal_expansion_temps
                    ,canal_enregistrement_direct=Seldatapart$configuration$canal_enregistrement_direct
                    ,micro0_type=Seldatapart$configuration$micro0_type
                    ,micro0_numero_serie=Seldatapart$configuration$micro0_numero_serie
                    ,micro0_position=Seldatapart$configuration$micro0_position
                    ,micro0_hauteur=Seldatapart$configuration$micro0_hauteur
                    ,micro1_type=Seldatapart$configuration$micro1_type
                    ,micro1_numero_serie=Seldatapart$configuration$micro1_numero_serie
                    ,micro1_position=Seldatapart$configuration$micro1_position
                    ,micro1_hauteur=Seldatapart$configuration$micro1_hauteur
                    ,commentaire=Seldatapart$commentaire
)

fwrite(p_export,"p_export.csv",sep=";")
fwrite(p_export,"p_export_forLinux.csv",sep=";")

head(p_export)

head(table(p_export$observateur)[order(table(p_export$observateur),decreasing = T)],10)
summary(p_export$sps)




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

#fwrite(ListPartECnew,paste0("ListPartECnew",Sys.Date(),".csv"),sep=";")


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

ListPartEall=subset(ListPartEall,!(ListPartEall %in% LPS$ListPartEall))

ListPartEall=as.data.frame(ListPartEall)

fwrite(ListPartEall,paste0("ListPartEall",Sys.Date(),".csv"),sep=";")


datapartP=subset(alldatapart,alldatapart$traitement$etat=="PLANIFIE")

test=match(datapartP$observateur,alldataobs$'_id')

table(alldataobs$pseudo[test])

print(min(datapartP$traitement$date_planification))





