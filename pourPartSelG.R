library(data.table)
#récupération des données participation
Particip=fread("C:/wamp64/www/p_export.csv")
#récupération des localités
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")


#liste des coordonnées existantes dans ce jeu de données
PartPF=subset(Particip,grepl("Fixe",Particip$site))
SelParSL=merge(SiteLoc,PartPF,by.x=c("site","nom"),by.y=c("site","point"))

SelParSL$Mois=as.numeric(substr(SelParSL$date_debut,4,5))

fwrite(SelParSL,"PartSelG.csv")
