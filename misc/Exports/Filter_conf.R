library(data.table)
library(readxl)

Utilisateurs=fread("C:/wamp64/www/utilisateurs.txt")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
Esel=read_xlsx("./Tadarida/rounds/valid2101/Esel_Barfis.xlsx")
LF=list.files("C:/wamp64/www/Barfis_sel",pattern=".wav$",recursive=T)

PrefLF=substr(basename(LF),1,24)
PrefE=substr(Esel$donnee,1,24)

Pe=subset(Particip,Particip$participation %in% Esel$participation)
MatchPSL=match(paste(Pe$site,Pe$point),paste(SiteLoc$site,SiteLoc$nom))
Pe$observateur_site=SiteLoc$observateur[MatchPSL]
Pe$auteur=ifelse((Pe$observateur %in% c("Yves Bas"
                                       ,"christian kerbiriou"))
                 &(!is.na(Pe$observateur_site))
                 ,Pe$observateur_site,Pe$observateur)
MatchPU=match(Pe$auteur,Utilisateurs$pseudo)
Pe$conf=Utilisateurs$confidentiel[MatchPU]
table(Pe$conf)


MatchF=match(PrefLF,PrefE)
MatchE=match(Esel$participation,Pe$participation)
ConfidF=Pe$conf[MatchE[MatchF]]
FConfid=subset(LF,ConfidF=="oui")
dir.create("C:/wamp64/www/Barfis_sel/Confid/")
NewConfid=paste0("C:/wamp64/www/Barfis_sel/Confid/",basename(FConfid))
OldConfid=paste0("C:/wamp64/www/Barfis_sel/",FConfid)
file.rename(from=OldConfid,to=NewConfid)


