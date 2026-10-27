library(mongolite)
library(data.table)
#library(beepr)
library(raster)
#library(rgdal)
#library(maptools)
#library(rgeos)
library(uuid)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(readxl)


mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
OutF="utilisateurs_raw.csv"
#OutF="C:/Users/yvesb/Documents/www/utilisateurs_test.csv"
DirEmbargo="C:/Users/ybas/Downloads/"
PatternEmbargo="Nouvelle Charte avec Embargo"
Var=c("_id","donnees_publiques","email","google_id" 
      ,"pseudo",             "role",                  "professionnel"
      ,"nom"             ,   "prenom"        ,     "adresse"          ,  "telephone"        ,  "organisation"   ,    "charte_acceptee"   
      ,"facebook_id" ,       "commentaire" ,"_created" )
LPS=fread("ListPartStuck.csv")
NbJourEC=1

FileEmbargo=list.files(DirEmbargo,PatternEmbargo,full.names=T)
FileEmbargoI=file.info(FileEmbargo)
FileEmbargo=FileEmbargo[order(FileEmbargoI$ctime)]
FileEmbargoSel=FileEmbargo[length(FileEmbargo)]
Embargo=read_xlsx(FileEmbargoSel)


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)

Sys.time()
#alldata <- users$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}')) #protocole PF
alldata <- users$find(fields='{}')
AllDataS=subset(alldata,select=Var)
Sys.time()

table(alldata$charte_acceptee)

ConfUsers=subset(alldata,alldata$donnees_publiques==F)
ConfUsers$email
OpenUsers=subset(alldata,alldata$donnees_publiques==T)
OpenUsersChelou=subset(OpenUsers,OpenUsers$charte_acceptee==F)
OpenUsersChelou$email

test40=match(AllDataS$email,Embargo$email)
summary(test40)
AllDataS$Embargo=ifelse(is.na(test40),F,T)
summary(AllDataS$Embargo)  

fwrite(AllDataS,OutF,sep=";")

test=subset(AllDataS,grepl("marion",AllDataS$email))
test$email



sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
#listpart=fread("ccsps220906.csv")
#testE=participations$export()
listpart=NA
users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)



Sys.time()
alldatapart<-participations$find(fields='{}') # ~1 sec / 1e3 participations
Sys.time()
head(alldatapart$date_debut)
test39=subset(alldatapart,alldatapart$date_debut==max(alldatapart$date_debut))
test40=subset(alldatapart,alldatapart$date_debut==min(alldatapart$date_debut))
test39$'_id'
test40$'_id'
alldatapartO=alldatapart[order(alldatapart$date_debut),]

dates <- as.POSIXct(as.numeric(alldatapart$date_debut), origin = "1970-01-01", tz = "UTC")
head(dates)
test=subset(alldatapart,is.na(dates))
test$'_id'
test42=participations$export(query='{ "_id" : { "$oid" : "69c399ca948142bbc706dd71" }}')

table(test$obs)
test41=subset(test,test$observateur=="62824756aeb33679fc8d21a8")
test41$'_id'
table(test$observateur)
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

#summary traitements

p_export$yearTraitement=year(p_export$trait_debut)
p_export$moisTraitement=month(p_export$trait_debut)
table(p_export$moisTraitement)
table(p_export$yearTraitement,is.na(p_export$duree_traitement))
p_export=subset(p_export,!is.na(p_export$duree_traitement))

table(p_export$yearTraitement)
ConsoParAn=aggregate(p_export$duree_traitement
                     ,by=list(p_export$yearTraitement),sum)


ConsoParAn$Heure=ConsoParAn$x/3600
ConsoParAn


ConsoParmois=aggregate(p_export$duree_traitement
                     ,by=c(list(p_export$moisTraitement)
                           ,list(p_export$yearTraitement)),sum)


ConsoParmois$Heure=ConsoParmois$x/3600
ConsoParmois



DataParAn=aggregate(p_export$nb_obs
                     ,by=list(p_export$yearTraitement),sum)
DataParAn


#p_export=subset(p_export,substr(p_export$date_debut,1,1)=="1")
#p_export=subset(p_export,substr(p_export$date_debut,1,1)=="1")

test225=try(year(p_export$date_debut))
if(class(test225)=="try-error"){
  dates <- format(
    as.POSIXct(as.numeric(p_export$date_debut), origin = "1970-01-01", tz = "UTC"),
    "%d/%m/%Y"
  )
table(is.na(dates))
subset(p_export$participation,is.na(dates))
subset(p_export$date,is.na(dates))
  table(year(dates))
  p_export$year=year(dates)
    
}else{
p_export$year=year(p_export$date_debut)
}

as.data.frame(table(p_export$year))

LogEstimateSize=-8.177+1.199*log(p_export$duree_traitement)
p_export$SizeEstimatedInGB=exp(LogEstimateSize)
sum(p_export$SizeEstimatedInGB)

SizeParAn=aggregate(p_export$SizeEstimatedInGB
                    ,by=list(p_export$yearTraitement),sum)
SizeParAn

SizeParAn=aggregate(p_export$SizeEstimatedInGB
                    ,by=list(p_export$year),function(x) round(sum(x)/1000))
SizeParAn
SizeParAn=subset(SizeParAn,SizeParAn$Group.1>2013)

p_export$CreationDate=substr(p_export$date,1,10)

SummaryUploadPerDay=aggregate(SizeEstimatedInGB~CreationDate,data=p_export,sum)
fwrite(SummaryUploadPerDay,"SummaryUploadPerDay.csv")
barplot(SummaryUploadPerDay$SizeEstimatedInGB)

SummaryUpload2025=subset(SummaryUploadPerDay,year(SummaryUploadPerDay$CreationDate)==2025)
barplot(SummaryUpload2025$SizeEstimatedInGB)
fwrite(SummaryUpload2025,"SummaryUpload2025.csv")
p<-ggplot(data=SizeParAn, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity")
p

fwrite(p_export,"p_export.csv",sep=";")
fwrite(p_export,"p_export_forLinux.csv",sep=";")

match318=match(p_export$idobservateur,AllDataS$`_id`)

p_export$charte_acceptee=AllDataS$charte_acceptee[match318]
summary(p_export$charte_acceptee)
test322=subset(p_export,is.na(p_export$charte_acceptee))
#table(test322$observateur)
p_export$charte_acceptee[is.na(p_export$charte_acceptee)]=T
p_export$Embargo=AllDataS$Embargo[match318]
summary(p_export$Embargo)
#A RECODER EN IMPLEMENTANT L'EMBARGO DE 3 ANS
fwrite(p_export,"p_export_infoconf.csv",sep=";")
p_export_open=subset(p_export,p_export$charte_acceptee&(!p_export$Embargo))
fwrite(p_export_open,"p_export_open.csv",sep=";")


length(unique(p_export$idobservateur))

pData=subset(p_export,p_export$nb_obs>0)
length(unique(pData$idobservateur))
SiteUnique=subset(p_export,select=c("site","point"))
SiteUnique=unique(SiteUnique)
CarreUnique=subset(p_export,select=c("site"))
CarreUnique=unique(CarreUnique)
table(nchar(CarreUnique$site))

tail(alldatapart$`_created`)

sum(p_export$nb_obs) #nb obs
sum(p_export$nb_obs)/2 #nb files estimated

