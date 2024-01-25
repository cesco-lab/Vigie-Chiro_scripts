library(data.table)

#r?cup?ration des donn?es participation
#Particip=fread("p_export_forLinux.csv")
Particip=fread("C:/Users/yvesb/Documents/www/p_export.csv")
Users=fread("C:/Users/yvesb/Documents/www/utilisateurs.txt")
#r?cup?ration des localit?s
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
FiltConfid=T
Wav=T
Dates=c(c(105:175))



if(FiltConfid)
{
# Charte=fread("C:/Users/Yves Bas/Downloads/R?ponses Charte Vigie-Chiro - Sheet1.csv",encoding="UTF-8")
# ListObsConfid=subset(Charte$'Identifiants concern?s',Charte$'Option choisie'==2)
UsersConfid=subset(Users,Users$confidentiel=="oui")
#UsersConfid=subset(Users,Users$confidentiel=="")

  head(UsersConfid)
  UsersOpen=subset(Users,Users$confidentiel=="non")
  }

#liste des coordonn?es existantes dans ce jeu de donn?es
PartPF=subset(Particip,grepl("Fixe",Particip$site))
if(FiltConfid)
{
  PartPF=subset(PartPF,!PartPF$observateur %in% UsersConfid$'_id')
}

if(Wav){PartPF=subset(PartPF,PartPF$nb_wav>0)}

SelParSL=merge(SiteLoc,PartPF,by.x=c("site","nom"),by.y=c("site","point"))


if(substr(SelParSL$date_debut[1],5,5)=="-"){
SelParSL$Mois=as.numeric(substr(SelParSL$date_debut,6,7))
barplot(table(SelParSL$Mois))
SelParSL$Jour=as.numeric(substr(SelParSL$date_debut,9,10))
barplot(table(SelParSL$Jour))
SelParSL$yday=(SelParSL$Mois*30)+SelParSL$Jour-30
barplot(table(SelParSL$yday))
}else{
SelParSL$Mois=as.numeric(substr(SelParSL$date_debut,4,5))
barplot(table(SelParSL$Mois))
SelParSL$Jour=as.numeric(substr(SelParSL$date_debut,1,2))
barplot(table(SelParSL$Jour))
SelParSL$yday=(SelParSL$Mois*30)+SelParSL$Jour-30
barplot(table(SelParSL$yday))
}

SelParSL=subset(SelParSL,SelParSL$yday %in% Dates)


if(FiltConfid)
{
fwrite(SelParSL,"PartSelGOpen.csv",sep=";")
}else{
  fwrite(SelParSL,"PartSelG.csv",sep=";")
  
}
