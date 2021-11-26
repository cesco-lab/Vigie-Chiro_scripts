library(data.table)
library(readxl)


f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 22, nchar(x)-8), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")
}

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
MatchP=match(paste(Pe$site,Pe$point),paste(SiteLoc$site,SiteLoc$nom))
Auteur=Pe$auteur[MatchE[MatchF]]
Long_WGS84=SiteLoc$longitude[MatchP[MatchE[MatchF]]]
Lat_WGS84=SiteLoc$latitude[MatchP[MatchE[MatchF]]]
Materiel=Pe$detecteur_enregistreur_type[MatchE[MatchF]]
table(Materiel)
DateHeure=f2pPF(basename(LF))
table(DateHeure$hour)
Fichier=basename(LF)
DataBarbitistes=data.frame(Fichier,Auteur,Long_WGS84,Lat_WGS84,DateHeure)
fwrite(DataBarbitistes,"DataBarbitistes.csv",sep=";")
