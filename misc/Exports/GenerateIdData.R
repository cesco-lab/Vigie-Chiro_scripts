library(data.table)
#library(raster)

FOccSpPF="C:/wamp64/www/SpNuit2Valid_0_PG.csv"
FOccSpRP="DataRP_SpSecteur_0_V.csv"
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.csv")
Utilisateurs=fread("C:/wamp64/www/utilisateurs.txt")
Years=c(2010,2021)
SpeciesList=fread("SpeciesList.csv")
IdDir="C:/wamp64/www"
FilterOutRP=T
DurEnr=fread("C:/wamp64/www/DurEnr.csv")
DMtot=fread("C:/wamp64/www/DMTot.csv")
DecProb=fread("C:/wamp64/www/DecProb.csv")


Idfiles=list.files(IdDir,pattern="Identifiants",full.names=T)
Idlist=list()
for (i in 1:length(Idfiles))
{
  Idlist[[i]]=fread(Idfiles[i])
}
IdLast=rbindlist(Idlist)

PSLPF=merge(Particip,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))

OccSpPF=fread(FOccSpPF)
test=match(OccSpPF$participation,PSLPF$participation)
OccSpPF$longitude=PSLPF$longitude[test]
OccSpPF$latitude=PSLPF$latitude[test]
OccSpPF$precision=25
OccSpPF$protocole="point fixe nuit entiere"
OccSpPF$type="enregistrement continu"
OccSpPF$enregistreur=PSLPF$detecteur_enregistreur_type[test]
OccSpPF$type_micro=PSLPF$micro0_type[test]
OccSpPF$hauteur_micro=PSLPF$micro0_hauteur[test]
OccSpPF$hauteur_micro=gsub(" ","",OccSpPF$hauteur_micro)
OccSpPF$hauteur_micro=gsub("m","",OccSpPF$hauteur_micro)
OccSpPF$hauteur_micro=gsub("M","",OccSpPF$hauteur_micro)
OccSpPF$hauteur_micro=gsub("environ","",OccSpPF$hauteur_micro)
OccSpPF$hauteur_micro=gsub(",","\\.",OccSpPF$hauteur_micro)
OccSpPF$hauteur_micro=as.numeric(OccSpPF$hauteur_micro)
summary(OccSpPF$hauteur_micro)

OccSpPF$gite=PSLPF$SpGite[test]
OccSpPF$nom_gite=PSLPF$nom_gite[test]

testD=match(paste(OccSpPF$participation,OccSpPF$Nuit)
            ,paste(DurEnr$Group.1,DurEnr$Group.2))
summary(testD)
print(paste("%age participations supprim?es :",
            round(sum(is.na(testD))/nrow(OccSpPF)*100,1)))
OccSpPF$nb_heures_enregistrement=DurEnr$duree_enregistrement[testD]
OccSpPF$nuit_complete=DurEnr$nuit_complete[testD]

testU=match(PSLPF$id_observateur,Utilisateurs$identifiant)
Pseudo=Utilisateurs$pseudo[testU[test]]
Nom=Utilisateurs$nom[testU[test]]
Prenom=Utilisateurs$prenom[testU[test]]
Id_O=Utilisateurs$identifiant[testU[test]]

NomE=(Nom!="")
PrenomE=(Prenom!="")
table(NomE,PrenomE)
table(subset(Nom,NomE&!PrenomE))
OccSpPF$observateur_rice=ifelse(PrenomE,paste(Prenom,Nom),Pseudo)
OccSpPF$id_obs=Id_O
OccSpPF$numero_carre=gsub("Vigiechiro - Point Fixe-","",PSLPF$site[test])
OccSpPF$point=PSLPF$point[test]
OccSpPF$temperature_debut=PSLPF$temperature_debut[test]
OccSpPF$temperature_fin=PSLPF$temperature_fin[test]
OccSpPF$couverture_nuageuse=PSLPF$couverture[test]
OccSpPF$vent=PSLPF$vent[test]
#tagger int?rieur gite / sorties gites
OccSpPF$type_suivi_gite=ifelse(OccSpPF$gite,ifelse(OccSpPF$hauteur_micro>=0
                                                   ,"sortie"
                                                   ,"interieur")
                               ,"")

OccSpPF$min_decalage_coucher=round(OccSpPF$min_decalage_coucher)
OccSpPF$min_decalage_lever=round(OccSpPF$min_decalage_lever)
OccSpPF$nb_heures_enregistrement=rond(OccSpPF$nb_heures_enregistrement,1)
summary(is.na(OccSpPF$indice_gite))
OccSpPF$indice_gite[is.na(OccSpPF$indice_gite)]=0
OccSpPF$indice_reposnocturne[is.na(OccSpPF$indice_reposnocturne)]=0


summary(is.na(OccSpPF$type_suivi_gite))
table((OccSpPF$type_suivi_gite))

testMic=match(paste(OccSpPF$participation,OccSpPF$num_micro)
              ,paste(DMtot$Group.1,DMtot$Group.2))
summary(is.na(testMic        ))
OccSpPF$probleme_micro=DMtot$probleme_micro[testMic]
table(OccSpPF$probleme_micro)

#VIRER dur?es n?gatives
OccSpPF=subset(OccSpPF,OccSpPF$nb_heures_enregistrement>=0)

#VIRER participations supprim?es
OccSpPF=subset(OccSpPF,!is.na(OccSpPF$nb_heures_enregistrement))

#VIRER participations non mise ? jour
PartMAJ=subset(DecProb$Group.1,DecProb$x<3)
OccSpPF=subset(OccSpPF,OccSpPF$participation %in% PartMAJ)


#UniquePF=subset(OccSpPF,select=c("espece","Nuit","longitude","latitude"))


if(!FilterOutRP){
  OccSpRP=fread(FOccSpRP)
  PRP=subset(Particip,!grepl("Fixe",Particip$site))
  PRP$Nuit=paste(substr(PRP$date_debut,7,10),substr(PRP$date_debut,4,5)
                 ,substr(PRP$date_debut,1,2),sep="-")
  PSLRP=merge(PRP,SiteLoc,by.x=c("site"),by.y=c("site"),allow.cartesian=TRUE)
  test=match(OccSpRP$participation,PSLRP$participation)
  Protocole=PSLRP$protocole[test]
  NomSecteur=ifelse(Protocole=="ROUTIER"
                    ,paste("T",OccSpRP$Tron,OccSpRP$Secteur)
                    ,OccSpRP$Tron)
  OccSpRP$Nuit=PSLRP$Nuit[test]
  OccSpRP$precision=ifelse(Protocole=="ROUTIER",200,25)
  OccSpRP$protocole=ifelse(Protocole=="ROUTIER"
                           ,"transect routier - section de 400 m"
                           ,"point fixe de 6 minutes")
  
  OccSpRP$type=ifelse(PSLRP$canal_expansion_temps[test]=="GAUCHE"&
                        OccSpRP$num_micro==F
                      ,"expansion de temps a declenchement automatique"
                      ,ifelse(PSLRP$canal_expansion_temps[test]=="DROITE"&
                                OccSpRP$num_micro==T
                              ,"expansion de temps a declenchement automatique"
                              ,ifelse(PSLRP$canal_enregistrement_direct[test]=="DROITE"&
                                        OccSpRP$num_micro==T
                                      ,"enregistrement continu"
                                      ,ifelse(PSLRP$canal_enregistrement_direct[test]=="GAUCHE"&
                                                OccSpRP$num_micro==F
                                              ,"enregistrement continu","inconnu"
                                      ))))
  
  OccSpRP$enregistreur=PSLRP$detecteur_enregistreur_type[test]
  OccSpRP$type_micro=PSLRP$micro0_type[test]
  OccSpRP$hauteur_micro=PSLRP$micro0_hauteur[test]
  
  
  table(NomSecteur)
  test2=match(paste(OccSpRP$participation,NomSecteur)
              ,paste(PSLRP$participation,PSLRP$nom))
  OccSpRP$longitude=PSLRP$longitude[test2]
  OccSpRP$latitude=PSLRP$latitude[test2]
  OccSpRP$obs=PSLRP$observateur.x[test2]
  table(OccSpRP$obs)[order(table(OccSpRP$obs))]
  OccSpRP=subset(OccSpRP,OccSpRP$type!="inconnu")
  OccSpRP$gite=0
  CommonNames=subset(names(OccSpPF),names(OccSpPF) %in% names(OccSpRP))
  OccSpPFs=subset(OccSpPF,select=CommonNames)
  OccSpRPs=subset(OccSpRP,select=CommonNames)
  OccTot=rbind(OccSpRPs,OccSpPFs)
}else{
  OccTot=OccSpPF  
}
names(OccTot)
OccTot$decalage_fin_lever=NULL
OccTot$decalage_debut_lever=NULL
OccTot$decalage_fin_coucher=NULL
OccTot$decalage_debut_coucher=NULL

barplot(table(substr(OccTot$Nuit,1,4)),las=2)
OccTot$annee=as.numeric(substr(OccTot$Nuit,1,4))
OccTot=subset(OccTot,OccTot$annee>=Years[1])
OccTot=subset(OccTot,OccTot$annee<=Years[2])
barplot(table(substr(OccTot$Nuit,1,4)),las=2)

Iid=paste(IdLast$espece,IdLast$Nuit,IdLast$longitude,IdLast$latitude)
Oid=paste(OccTot$espece,OccTot$Nuit,OccTot$longitude,OccTot$latitude)

test=match(Oid,Iid)
Idinit=max(IdLast$idnum,0)
OccTot$idnum=IdLast$idnum[test]
OccOld=subset(OccTot,!is.na(OccTot$idnum))
OccNew=subset(OccTot,is.na(OccTot$idnum))
if(nrow(OccNew)>0)
{
  OccNew$idnum=c((Idinit+1):(Idinit+nrow(OccNew)))
  OccTot=rbind(OccOld,OccNew)
}

fwrite(OccTot,"C:/wamp64/www/DataToti.csv",sep=";")

IdToday=subset(OccTot,select=c("espece","Nuit","longitude","latitude"
                               ,"idnum"))
fwrite(IdToday,paste0(IdDir,"/Identifiants",Sys.Date(),".csv"),sep=";")

#filteroutconf
test=subset(Utilisateurs
            ,Utilisateurs$identifiant=="5a61fffea7644b000d4a8e19")
UClose=subset(Utilisateurs,Utilisateurs$confidentiel=="oui")
test1=subset(UClose
             ,UClose$identifiant=="5a61fffea7644b000d4a8e19")

UOpen=subset(Utilisateurs,Utilisateurs$confidentiel!="oui")
test1b=subset(UOpen
              ,UOpen$identifiant=="5a61fffea7644b000d4a8e19")

test2=("5a61fffea7644b000d4a8e19" %in% UOpen$identifiant)

test=("5ab8eb985d4d3fa86e0003ab" %in% OccTot$participation)

OccOpen=subset(OccTot,OccTot$id_obs %in% UOpen$identifiant)
test2=("5ab8eb985d4d3fa86e0003ab" %in% OccOpen$participation)
#OccOpen=subset(OccOpen,OccOpen$score_max>=0.5)
print(paste("perte donnees confidentielles :"
            ,(1-nrow(OccOpen)/nrow(OccTot))*100))
test=match(OccOpen$espece,SpeciesList$Esp)
summary(is.na(test))
OccOpen$NomFr=SpeciesList$NomFR[test]
OccOpen$NomSci=SpeciesList$`Scientific name`[test]


fwrite(OccOpen,"C:/wamp64/www/DataOpeni.csv",sep=";")

#filterout sensitive places TO DO
OccNS=subset(OccOpen,OccOpen$gite==0)

fwrite(OccNS,"C:/wamp64/www/DataNS.csv",sep=";")


