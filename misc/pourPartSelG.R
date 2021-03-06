library(data.table)
#r�cup�ration des donn�es participation
Particip=fread("C:/wamp64/www/p_export.csv")
#r�cup�ration des localit�s
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
FiltConfid=F
if(FiltConfid)
{
Charte=fread("C:/Users/Yves Bas/Downloads/R�ponses Charte Vigie-Chiro - Sheet1.csv",encoding="UTF-8")
ListObsConfid=subset(Charte$'Identifiants concern�s',Charte$'Option choisie'==2)
}

#liste des coordonn�es existantes dans ce jeu de donn�es
PartPF=subset(Particip,grepl("Fixe",Particip$site))
if(FiltConfid)
{
  PartPF=subset(PartPF,!PartPF$observateur %in% ListObsConfid)
  }

SelParSL=merge(SiteLoc,PartPF,by.x=c("site","nom"),by.y=c("site","point"))

SelParSL$Mois=as.numeric(substr(SelParSL$date_debut,4,5))
SelParSL$Jour=as.numeric(substr(SelParSL$date_debut,1,2))
SelParSL$yday=(SelParSL$Mois*30)+SelParSL$Jour-30

if(FiltConfid)
{
fwrite(SelParSL,"PartSelGOpen.csv",sep=";")
}else{
  fwrite(SelParSL,"PartSelG.csv",sep=";")
  
}
