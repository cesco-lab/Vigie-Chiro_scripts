library(data.table)


PG=fread("C:/Users/yvesb/Documents/www/SpNuit2Valid_50_PG.csv")
SpTarget="Tadten"
Particip=fread("p_export.csv")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")

PGsp=subset(PG,PG$espece==SpTarget)

testP=match(PGsp$participation,Particip$participation)
testS=match(paste(Particip$idsite,Particip$point),paste(SiteLoc$id_site,SiteLoc$nom))
PGsp$longitude=SiteLoc$longitude[testS[testP]]
PGsp$latitude=SiteLoc$latitude[testS[testP]]

head(PGsp)

fwrite(PGsp,paste0("PG_",SpTarget,".csv"),sep=";")

