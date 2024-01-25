library(data.table)

GI=fread("C:/Users/yvesb/Documents/www/GI_FR_sites_localites_PF.csv")
SL=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
Particip=fread("p_export.csv")

testS=match(paste(SL$longitude,SL$latitude),paste(GI$longitude,GI$latitude))
summary(testS)
summary(is.na(testS))
testP=match(paste(Particip$site,Particip$point),paste(SL$site,SL$nom))
summary(testP)
GIP=GI[testS[testP],]
GIP$participation=Particip$participation
fwrite(GIP,"GIP.csv",sep=";")
