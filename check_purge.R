library(data.table)
Apa=fread("C:/wamp64/www/apurgeraussi.txt",h=F)
Particip=fread("C:/wamp64/www/p_export.csv")

Ppurge=subset(Particip,Particip$participation %in% Apa$V3)
print(table(Ppurge$trait_etat))
