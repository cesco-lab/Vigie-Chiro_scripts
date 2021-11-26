###
# Durée de séquence complète
###

# Ce script permet de calculer la durée de séquence complète : permet d'agréger
# des séquences de chiroptères quand elles se suivent à moins de x seconde 
# (x à choisir en arg[4]) par espèce

# fichier de sortie : 
# Numéro de séquence
# Espèce
# TempsDebutSequence : temps de début de la séquence
# TempsFinSequence : temps de fin de la séquence
# DureeSequence : durée de la séquence en seconde

arg = "5b7eeaecd2aa6b000e7505d4" #participation choisie
arg[2] = "D:/VigieChiro_RawData/export_all/" #répertoire avec les données brutes
arg[3] = "C:/Users/sarah/Desktop/Stage/duree_seq/f2pPF.R" #chemin pour la fonction f2pPF
arg[4] = as.numeric(2) #critère de seconde de silence (par défaut 2)
arg[5] = "C:/Users/sarah/Desktop/Stage/duree_seq/liste_espece.txt" #chemin pour la liste des espèces
arg[6] = "C:/Users/sarah/Desktop/Stage/duree_seq/" #répertoire pour le fichier final


library(stringr)
library(dplyr)
library(data.table)
library(lubridate)
source(arg[3])

fichier <- str_c(arg[2],"export_",substr(arg[1],1,3),".csv")
esp_tot <- fread(fichier, check.names = T)

# Récupérer les espèces correspondant à la participation choisie
esp_tot=as.data.frame(esp_tot)
esp_part <- esp_tot[which(esp_tot$participation==arg[1]),]
#esp_part <- subset(esp_tot,esp_tot$participation==arg[1])

#Rajouter les dates avec la fonction f2pPF
esp_part$date_esp <- f2pPF(esp_part$donnee)
esp_part=esp_part[order(esp_part$date_esp),]

#filtrer uniquement les chiros
list_esp <- read.table(arg[5], header = T, sep = ";", quote = "", 
                       row.names = NULL, stringsAsFactors = FALSE, 
                       encoding = "UTF-8")

#Mettre tous les noms d'espèce en minuscule (pbm de compatibilité des 2 fichiers)
list_esp$Esp <- str_to_lower(list_esp$Esp)
esp_part$espece <- str_to_lower(esp_part$espece)
esp_part <- inner_join(esp_part, list_esp[,c("Esp","Group")], by = c("espece"="Esp"))

esp_part <- subset(esp_part, esp_part$Group=="bat" )

ListespP=unique(esp_part$espece)
esp_part$NumeroSequence=NA

compteur=0
espSeqAggr=data.frame()
for (i in 1:length(ListespP))
{
  espi=subset(esp_part,esp_part$espece==ListespP[i]) #selectionne l'espece
  compteur=compteur+1 #a chaque fois qu'on change d'espece, on change forcément de séquence
  print(paste(ListespP[i],nrow(espi),Sys.time())) #infos diverses
  if(nrow(espi)==1){ #cas où 1 seul fichier pour l'espèce
    espi$NumeroSequence=compteur 
  }else{ #cas où il y a plusieurs fichiers pour l'espèce
    for (j in 1:(nrow(espi)-1)) #on compare une ligne avec la suivante donc on s'arrete sur l'avant dernière
    {
      espi$NumeroSequence[j]=compteur 
      TimeEnd=espi$date_esp[j]+espi$temps_debut.1[j] #temps fin de la séquence
      TimeStartNext=espi$date_esp[j+1]+espi$temps_debut[j+1] #temps début de la suivante
      if(TimeStartNext>(TimeEnd+as.numeric(arg[4]))) #si plus de 2 secondes de "trou" entre cette séquence et la suivante, on décide que c'est une autre séquence, sinon le compteur ne change pas (même séquence)
      {
        compteur=compteur+1 
      }
    }
    espi$NumeroSequence[nrow(espi)]=compteur #remplir l'info pour la dernière ligne (pas de comparaison)
  }
  espSeqAggr=rbind(espSeqAggr,espi) #rajouter les données de l'espèce à la table totale
}
length(unique(espSeqAggr$NumeroSequence)) #nombre total de séquences 
aggregate(espSeqAggr$NumeroSequence,by=list(espSeqAggr$espece)
          ,FUN=function(x) (length(unique(x)))) #bilan du nombre de séquence par espèce
table(espSeqAggr$espece) #nombre de fichiers par espèce (pour comparaison)

# Calcul de la durée de chaque séquence

especeSequence = aggregate(espSeqAggr$espece, by = list(espSeqAggr$NumeroSequence), 
                           unique)

espSeqAggr$TimeStart=espSeqAggr$date_esp+espSeqAggr$temps_debut
espSeqAggr$TimeEnd=espSeqAggr$date_esp+espSeqAggr$temps_debut.1

TempsDebutSequence=aggregate(espSeqAggr$TimeStart
                             ,by=list(espSeqAggr$NumeroSequence)
                             ,min)
TempsFinSequence=aggregate(espSeqAggr$TimeEnd
                           ,by=list(espSeqAggr$NumeroSequence)
                           ,max)
DureeSequence=as.numeric(TempsFinSequence$x-TempsDebutSequence$x)


##
# Fichier final 
##

fichier_final <- data.frame(NumeroSequence = unique(espSeqAggr$NumeroSequence),
                            Espece = especeSequence[,2],
                            TempsDebutSequence = TempsDebutSequence[,2], 
                            TempsFinSequence = TempsFinSequence[,2], 
                            DureeSequence)

write.csv(fichier_final, paste0(arg[6], "duree_seq_", arg[1],".csv"), row.names = F)