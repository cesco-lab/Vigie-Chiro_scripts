library(data.table)
PasRecupAR=fread("C:/wamp64/www/PasRecupARelancer.csv")
ListeTCZdispo=fread("C:/wamp64/www/listetcz.txt",header=F)
ListeTAZdispo=fread("C:/wamp64/www/listetaz.txt",header=F)


PasRecupARelancerVRAI=subset(PasRecupAR,!PasRecupAR$PasRecupARelancer %in% ListeTCZdispo$V1)
PasRecupARelancerTA=subset(PasRecupARelancerVRAI,PasRecupARelancerVRAI$PasRecupARelancer %in% ListeTAZdispo$V1)
PasRecupARelancerWAV=subset(PasRecupARelancerVRAI,!PasRecupARelancerVRAI$PasRecupARelancer %in% ListeTAZdispo$V1)


PasRecupSPSTCZ=subset(PasRecupAR,PasRecupAR$PasRecupARelancer %in% ListeTCZdispo$V1)

fwrite(PasRecupARelancerVRAI,"C:/wamp64/www/PasRecupARelancerVRAI.csv")
fwrite(PasRecupSPSTCZ,"C:/wamp64/www/PasRecupSPSTCZ.csv")
fwrite(PasRecupARelancerTA,"C:/wamp64/www/PasRecupARelancerTA.csv")
fwrite(PasRecupARelancerWAV,"C:/wamp64/www/PasRecupARelancerWAV.csv")

