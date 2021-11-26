library(data.table)
library(lubridate)

NewPar=fread("./www/NewPart2021-07-12.csv",sep=";")

colDD="date_debut"
colDF="date_fin"

Pimp1=NewPar

Pimp1$site=paste0("Vigiechiro - Point Fixe-",Pimp1$Carre)

InfoDD=subset(Pimp1,select=colDD)
names(InfoDD)="date_debut"
InfoDF=subset(Pimp1,select=colDF)
names(InfoDF)="date_fin"


if(nchar(InfoDD$date_debut[1])==10){
  InfoDD$date_debut=paste(InfoDD$date_debut,"12:00")
  InfoDF$date_fin=paste(InfoDF$date_fin,"12:00")
  }

InfoDD$date_debut=dmy_hm(InfoDD$date_debut)
Pimp1$date_debut=NULL
Pimp1=cbind(Pimp1,InfoDD)
Pimp1$date_debut=format(Pimp1$date_debut,format="%d/%m/%Y %H:%M")
Pimp1$date_debut=as.character(Pimp1$date_debut)
head(Pimp1$date_debut)

#Pimp1$date_debut=as.character(Pimp1$date_debut)


InfoDF$date_fin=dmy_hm(InfoDF$date_fin)
head(InfoDF)
Pimp1$date_fin=NULL
Pimp1=cbind(Pimp1,InfoDF)
Pimp1$date_fin=format(Pimp1$date_fin,format="%d/%m/%Y %H:%M")
Pimp1$date_fin=as.character(Pimp1$date_fin)

head(Pimp1$date_fin)

Pimp1$point=Pimp1$Point
table(Pimp1$point)
table(Pimp1$temperature_debut)
Pimp1$temperature_debut[Pimp1$temperature_debut=="/"]=""
table(Pimp1$temperature_fin)
Pimp1$temperature_fin[Pimp1$temperature_fin=="/"]=""
table(Pimp1$couverture)
Pimp1$couverture[Pimp1$couverture=="/"]=""
table(Pimp1$vent)
Pimp1$vent[Pimp1$vent=="/"]=""
Pimp1$type_micro_gauche=Pimp1$`Type de micro`
Pimp1$type_micro_droit=Pimp1$`Type de micro`
table(Pimp1$type_micro_gauche)
Pimp1$type_micro_gauche=gsub("Mic ","",Pimp1$type_micro_gauche)
table(Pimp1$type_micro_droit)
Pimp1$type_micro_droit=ifelse(is.na(Pimp1$'Haut Mic Right')
                              ,"",gsub("Mic ","",Pimp1$type_micro_droit))

Pimp1$commentaire=ifelse(Pimp1$`Trig Left`==6,Pimp1$commentaire
                         ,paste(Pimp1$commentaire,"Config Trig"
                                ,Pimp1$`Trig Left`,"dB"))
table(Pimp1$commentaire)
table(Pimp1$type_micro_gauche)
table(Pimp1$type_micro_droit)

Pimp1$position_micro_gauche=ifelse(Pimp1$`Haut Mic Left`>=10,"CANOPEE"
                                   ,"SOL")
Pimp1$position_micro_gauche[is.na(Pimp1$position_micro_gauche)]=""
table(Pimp1$position_micro_gauche)
Pimp1$position_micro_droit=ifelse(Pimp1$`Haut Mic Right`>=10,"CANOPEE"
                                   ,"SOL")
Pimp1$position_micro_droit[is.na(Pimp1$position_micro_droit)]=""
table(Pimp1$position_micro_droit)
Pimp1$numero_serie_micro_gauche=""
Pimp1$numero_serie_micro_droit=""
Pimp1$C20=""
Pimp1$C21=""
Pimp1$hauteur_micro_gauche=as.character(Pimp1$`Haut Mic Left`)
Pimp1$hauteur_micro_droit=as.character(Pimp1$`Haut Mic Right`)

Pimp1$detecteur_enregistreur_numero_serie=Pimp1$'detecteur_enregsitreur_numero-serie'

table(Pimp1$Détecteur)
Pimp1$detecteur_enregistreur_type=Pimp1$Détecteur


Pimp2=subset(Pimp1,select=c("site","date_debut","date_fin","point"
                            ,"commentaire","temperature_debut"
,"temperature_fin","couverture","vent","detecteur_enregistreur_type"
,"detecteur_enregistreur_numero_serie","type_micro_gauche"
,"hauteur_micro_gauche"
,"position_micro_gauche","numero_serie_micro_gauche","type_micro_droit"
,"hauteur_micro_droit"
,"position_micro_droit","numero_serie_micro_droit","C20","C21"))
Pimp2[is.na(Pimp2)]=""
fwrite(Pimp2,"pimport.csv",sep=";")
