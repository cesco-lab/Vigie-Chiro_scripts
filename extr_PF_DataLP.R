library(data.table)
library(StreamMetabolism)

#args="export_55.csv"

#ETAPE 0 - IMPORT DES TABLES
#bien renommer les chemins en fonction de l'ordi utilisé
#et vérifier les versions (date, import complet ou non)

#table "données"
DataE=fread(args[1])
Sys.time()
DataPF=subset(DataE,substr(DataE$donnee,1,3)=="Car")
Sys.time()
rm(DataE)

#table "participations"
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
Particip=as.data.frame(Particip)
#table "localités"
SiteLoc=fread("C:/wamp64/www/sites_localites.txt",sep="\t")

LatMin=0
LatMax=90
LongMin=-180
LongMax=180


f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

microdroitPF<-function(x)
{
  substr(x,nchar(x)-20,nchar(x)-20)=="1"
  
}

microPF<-function(x)
{
  substr(x,nchar(x)-20,nchar(x)-20)
}


#pour afficher les milisecondes
op <- options(digits.secs=3)
#pour reset
#options(op)


#ETAPE 1 - formattage des tables et de leurs attributs

#merge Localites et participations
PartProt=substr(Particip$site,1,22)
table(PartProt)
PartPF=subset(Particip,PartProt=="Vigiechiro - Point Fix")
table(PartPF$point)
length(subset(PartPF$point,substr(PartPF$point,1,1)!="Z"))
LocaPart=merge(SiteLoc,PartPF,by.x=c("site","nom"),by.y=c("site","point"))
print(paste("nb points manquants :",nrow(PartPF)-nrow(LocaPart),"/",nrow(PartPF)))


#DataE=subset(DataE,DataE$espece!="noise")
colnames(DataPF)[10]="temps_fin"
LocaPartData=as.factor(substr(DataPF$donnee,1,27)) #récupération de l'identifiant du point/tronçon
Sys.time()
#Datamicro=as.character(sapply(DataPF$donnee,FUN=microdroitPF)) # récupération du numéro du micro (4 min)
Sys.time()
Datamicro2=sapply(DataPF$donnee,FUN=microPF) # récupération du numéro du micro (4 min)
Sys.time()
Datamicro2[is.na(as.numeric(Datamicro2))]=0
#DataPF$Datamicro2=Datamicro2
#test=dcast(DataPF,participation~Datamicro2,fun.aggregate=length)
Sys.time()
DataMic0=aggregate(Datamicro2,by=list(DataPF$participation)
                   ,FUN=function(x) sum(x=="0"))
Sys.time()
DataMicL=aggregate(Datamicro2
                   ,by=c(list(DataPF$participation),list(Datamicro2))
                   ,FUN=length)
Sys.time()
DataMicN=aggregate(DataMicL$Group.2
                   ,by=list(DataMicL$Group.1)
                   ,FUN=function(x) max(as.numeric(as.character(x))))
Sys.time()
#test=match(DataMic0$Group.1,DataMicN$Group.1)
#plot(test,c(1:length(test)))
DataMic0$probleme=((DataMic0$x==0)|(DataMicN$x>3))
test=match(DataPF$participation,DataMic0$Group.1)
ProblemeMic=DataMic0$probleme[test]
DataPF$DataMicFinal=as.numeric(Datamicro2)*(1-ProblemeMic)

#TempsEnregistrement2=sapply(DataPF$donnee,FUN=f2pPF) #long à tourner

DataSel2=cbind(DataPF,LocaPartData)
rm(DataPF)
rm(LocaPartData)
rm(Datamicro2)

#calcul du temps (en secondes depuis 01/01/1970)
Sys.time()
TempsEnregistrement2=sapply(DataSel2$donnee,FUN=f2pPF) #long à tourner (3e5 données/min)
test=subset(DataSel2,is.na(TempsEnregistrement2))
Partbug=levels(as.factor(test$participation))
fwrite(as.data.frame(Partbug),paste0("Partbug_PF_LP_",args[1]))
Sys.time()
pourDateNuit=TempsEnregistrement2-12*3600 #bricolage-décalage de 12 heures pour ramener à la date du début de nuit
Sys.time()
DateNuit=as.Date.POSIXct(pourDateNuit) # date of the beginning of the night
Sys.time()
DateJour=as.Date.POSIXct(TempsEnregistrement2) # date (UTC+0)
DataSel2$TempsEnregistrement2=TempsEnregistrement2
DataSel2$DateNuit=DateNuit
DataSel2$DateJour=DateJour
rm(DateJour)
rm(DateNuit)
rm(TempsEnregistrement2)
rm(pourDateNuit)
Sys.time()
gc()


bugmatch=match(LocaPart$participation,Partbug)
LocaPart2=subset(LocaPart,is.na(bugmatch))

Sys.time()
DataLP=merge(DataSel2,LocaPart2,by="participation") #35 sec
Sys.time()
rm(DataSel2)
gc()

#purge des champs inutiles pour gagner de la mémoire (à remonter ?)
ListePurge=c("proprietaire","num site",
             "observateur.x",
             "email.y",
             "id_protocole",
             "protocole",
             "localite",
             "date.y",
             "observateur.y",
             "nb_wav",
             "nb_ta",
             "nb_tc",
             "dif_wav_ta",
             "pourc_dif",
             "trait_fin",
             "detecteur_enregistreur_numero_serie",
             "canal_expansion_temps",
             "canal_enregistrement_direct",
             "micro0_numero_serie",
             "micro1_numero_serie",
             "commentaire")
Sys.time()
DataLP[,(ListePurge):=NULL]
Sys.time()

Sys.time()
fwrite(DataLP,paste0("DataLP_PF_",args[1]),row.names=F) # 1 min
Sys.time()


