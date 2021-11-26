library(data.table)

SpTarget="Myocap"
Particip=fread("C:/wamp64/www/p_export.csv",encoding = "UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
RefGites=fread("C:/wamp64/www/RefGites2.csv")

RefGiteSpT=subset(RefGites,RefGites$espece==SpTarget)
GiteSpT=subset(RefGiteSpT,RefGiteSpT$probabiliteGite>0.5)

ParT=subset(Particip,Particip$participation %in% GiteSpT$participation)

SL_T=subset(SiteLoc
            ,paste(SiteLoc$site,SiteLoc$nom) %in% paste(ParT$site,ParT$point))

fwrite(SL_T,paste0("Gite_",SpTarget,".csv"),sep=";")
            