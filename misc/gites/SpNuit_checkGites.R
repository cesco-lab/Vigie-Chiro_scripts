library(data.table)

Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.csv")
SpNuit=fread("C:/wamp64/www/SpNuit2_0_DataLP_PF_exportTot.csv")

PartSL=merge(Particip,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
PartG=subset(PartSL,PartSL$SpGite==1)
PartG$randorder=sample.int(nrow(PartG))

test=match(SpNuit$participation,PartG$participation)

SpNuit$site=PartG$site[test]
SpNuit$point=PartG$point[test]
SpNuit$randorder=PartG$randorder[test]

SpNuit=subset(SpNuit,!is.na(SpNuit$site))
SpNuit=SpNuit[order(SpNuit$randorder),]
fwrite(SpNuit,"C:/wamp64/www/SpNuit_Gites.csv",sep=";")
