library(data.table)
#récupération des données participation
Particip=fread("C:/wamp64/www/p_export.csv")
#récupération des localités
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
FiltConfid=T
if(FiltConfid)
{
Charte=fread("C:/Users/Yves Bas/Downloads/Réponses Charte Vigie-Chiro - Sheet1.csv",encoding="UTF-8")
ListObsConfid=subset(Charte$'Identifiants concernés',Charte$'Option choisie'==2)
}

#liste des coordonnées existantes dans ce jeu de données
PartPF=subset(Particip,grepl("Fixe",Particip$site))
if(FiltConfid)
{
  PartPF=subset(PartPF,!PartPF$observateur %in% ListObsConfid)
  }

SelParSL=merge(SiteLoc,PartPF,by.x=c("site","nom"),by.y=c("site","point"))

SelParSL$Mois=as.numeric(substr(SelParSL$date_debut,4,5))

if(FiltConfid)
{
fwrite(SelParSL,"PartSelGOpen.csv",sep=";")
}else{
  fwrite(SelParSL,"PartSelG.csv",sep=";")
  
}