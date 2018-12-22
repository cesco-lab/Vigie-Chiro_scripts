packages <- c("dplyr", "ggplot2", "ggvis", "shiny","data.table","lubridate")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggvis)
library(shiny)
library(data.table)
library(lubridate)
# options(graphics.record = TRUE)
# windows(record = TRUE)
SpeciesList=fread("SpeciesList.csv")

#setwd("C:/Sonochiro") ## R?pertoire de travail. A ADAPTER !
# Pour libérer un peu de mémoire, décommenter les 3 lignes suivantes:
# liste <- subset(ls(), !grepl("batcalls", ls()))
gc()
groupes=unique(SpeciesList$Nesp)
especes=unique(SpeciesList$Esp)

#Chargement du fichier de sortie Sonochiro Biotope
print("CHARGEMENT DE LA PARTICIPATIOn A ANALYSER")
dirTC="./5ac3d1322fcbdb000fe885e9"
print(dirTC)
print("Choix du répertoire contenant les sons au format *.wav")
wavdir <- "C:/wamp64/www"
##### Chargement du dirTC
fichiersTC=list.files(dirTC,full.names=T)
my.data=list()
for(i in 1:length(fichiersTC))
{
  my.data[[i]]=fread(fichiersTC[i])
}


AlleYoupi5=rbindlist(my.data)

# Pour prendre en compte les wav enregistr?s directement et pas "from wac". 4 caract?res de moins (pas de "_nnn" millisecondes) donc ms=4; devrait fonctionner avec l'EM3
if   (substr(AlleYoupi5$Group.1[2],nchar(AlleYoupi5$Group.1[2])-7,nchar(AlleYoupi5$Group.1[2])-7) !="_") ms = 4 else ms = 0
####### Construction de la colonne DateHeure compatible POSIX
DateHeure <- substr(AlleYoupi5$Group.1,nchar(AlleYoupi5$Group.1)-22+ms,nchar(AlleYoupi5$Group.1)-8+ms)
DateHeure=ymd_hms(DateHeure) 
  
  
  
#AlleYoupi5 <- cbind(AlleYoupi5, DateHeure)
AlleYoupi5$DateHeure=DateHeure
AlleYoupi5$Date_nuit=as.Date(DateHeure+12*3600)

tab.synt<-data.frame(table(AlleYoupi5$SpMaxF2))
tab.synt<-tab.synt[order(tab.synt$Freq,decreasing=T),]
print ("#########################################################", quote = FALSE)
print(paste("fichier de données: ", dirTC, sep=""), quote = FALSE )
print(paste("dossiers des sons WAV: ", wavdir, sep=""), quote = FALSE )
print ("#########################################################", quote = FALSE)
cat(rep("\n",2))
print ("Nombre de contacts par ID:", quote = FALSE)
print(tab.synt)
print ("Nombre de contacts par ID(lignes) par indice de confiance(colonnes):", quote = FALSE)
tab.synt3<-table(AlleYoupi5$SpMaxF2,round(AlleYoupi5$SuccessProb,1))
print ("#########################################################", quote = FALSE)
cat(rep("\n",2))
print ("Nombre de contacts par Espèce(lignes) par indice de confiance(colonnes):", quote = FALSE)
print(tab.synt3)
print ("#########################################################", quote = FALSE)
cat(rep("\n",2))
print ("Nombre de contacts par Espèce(lignes) par Nuit (colonnes):", quote = FALSE)
tab.synt4<-table(AlleYoupi5$SpMaxF2,AlleYoupi5$Date_nuit)
print(tab.synt4)
##########  RETENIR pour une autre fois     as.data.frame.matrix()    pour transformer la table en data.frame. C'est la seule solution simple !! ######
##########  Fonction pour calculer l'heure de la nuit, après minuit > 24h
ampm <- function(x)
  x -as.integer(x>=12)*12 +as.integer(x<12)*12
##########
AlleYoupi5$Heure=hour(DateHeure)
AlleYoupi5$Heure_Midi <- ampm(AlleYoupi5$Heure)
tab.synt5<-table(AlleYoupi5$SpMaxF2,AlleYoupi5$Heure_Midi,deparse.level = 0)
actparheure <- as.data.frame.matrix(tab.synt5)
## remet les heures "après midi en heures légales; affiche la table de contingence activité par heure.
heuresmidi <- as.numeric(colnames(actparheure))
heureslegales <- as.factor(ampm(heuresmidi))
colnames(actparheure) <- heureslegales
print ("#########################################################", quote = FALSE)
cat(rep("\n",2))
print ("Nombre de contacts par Espèce(lignes) par heure (moyennes sur toutes les nuits):", quote = FALSE)
print (actparheure)
# ContactsHeure=paste(dirTC,"ParHeure.csv", sep="")
# write.table(actparheure,ContactsHeure,sep=",",row.names=T,col.names=NA)
#shell.exec(ContactsHeure)

#Création de variables pour l'app Shiny
AlleYoupi5$Affiche <- paste(AlleYoupi5$Group.1, " sp: ", AlleYoupi5$SpMaxF2, "Confiance: ", as.character(round(AlleYoupi5$SuccessProb,1), sep=""))
params <- c("NbCris", "Duree", "FreqC", "Ampm90", "AmpSMd","DiffME", "Order")
AlleYoupi5=as.data.frame(AlleYoupi5)
AlleYoupi7 <- cbind(Grpe_corrige = NA, Esp_corrige = NA, AlleYoupi5)
AlleYoupi8 <- AlleYoupi7[0, ]
submit0 <- 0
gpnames <-append("Tous", sort(as.character(unique(AlleYoupi5$Groupe))))
spnames <-append("Toutes", sort(as.character(unique(AlleYoupi5$SpMaxF2))))
timespan <- max(DateHeure) - min(DateHeure)
sliderlabel <- paste("Intervalle depuis: ", min(AlleYoupi5$DateHeure), "  jusqu'à ", max(AlleYoupi5$DateHeure), sep = "")
mintemps <- min(DateHeure)
maxtemps <- max(DateHeure)
fichierslash <- gsub("\\\\", "/", dirTC)
coupe <- unlist(strsplit(fichierslash,"/"))
titre <- substr(coupe[length(coupe)], 1, nchar(coupe[length(coupe)])-4)
fichiervu <- paste(titre, '_Vu.csv', sep = '')
runApp("C:/Users/Yves Bas/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/AppShiny", launch.browser = TRUE)
write.csv2(AlleYoupi7,"AlleYoupi7.csv")
# todo:
# 1) contacts secondaires,
# OK 2) correction des SpMaxF2 via shiny,
# 3) détection des fichiers si dans le même dossier que le log ScanR.
# 4) plusieurs tabs avec input (upload) du log ScanR et
# lancement de Sonochiro depuis shiny.


