library(readxl)
library(data.table)

NouvellesValidationsGite=read_xlsx("C:/wamp64/www/RefGitesNonValid2.xlsx")
PrecedentesValidationsGite=read_xlsx("C:/wamp64/www/SpNuit_Gites_qualif.xlsx")

ToAdd=subset(NouvellesValidationsGite,select=names(PrecedentesValidationsGite))
PrecV=subset(PrecedentesValidationsGite
             ,!is.na(PrecedentesValidationsGite$gite_diurne))
ValidationsGite=rbind(PrecV,ToAdd)
fwrite(ValidationsGite,"C:/wamp64/www/ValidationsGite.csv",sep=";")
