library(data.table)
Particip=fread("file:///C:/wamp64/www/p_export.csv")
LogSPS=fread("file:///C:/wamp64/www/logsps.txt")
LT=fread("file:///C:/wamp64/www/listetaz.txt")


PartSPS=subset(Particip,Particip$participation %in% LogSPS$`Directory or file in ""/sps/mnhn""`)
PartSPS_RP=subset(PartSPS,!grepl("Fixe",PartSPS$site))
fwrite(PartSPS_RP,"PartSPS_RP.csv",sep=";")

PartnoSPS=subset(Particip,!(Particip$participation %in% LogSPS$`Directory or file in ""/sps/mnhn""`))
PartnoSPS_RP=subset(PartnoSPS,!grepl("Fixe",PartnoSPS$site))

LT$participation=gsub(".tar.gz","",LT$V11)

LTsmall=subset(LT,as.numeric(LT$V7)<1000)
hist(as.numeric(LTsmall$V7),breaks=100)
PartnoSPS_RP_taz=subset(PartnoSPS_RP,PartnoSPS_RP$participation %in% LT$participation)
PartnoSPS_RP_notaz=subset(PartnoSPS_RP,!PartnoSPS_RP$participation %in% LT$participation)
fwrite(PartnoSPS_RP_taz,"PartnoSPS_RP_taz.csv",sep=";")
fwrite(PartnoSPS_RP_notaz,"PartnoSPS_RP_notaz.csv",sep=";")
