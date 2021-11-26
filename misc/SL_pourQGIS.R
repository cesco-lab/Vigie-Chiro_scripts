library(data.table)

SiteLoc=fread("./www/sites_localites.txt")
Particip=fread("./www/p_export_forLinux.csv")

PartSelG=merge(SiteLoc,Particip,by.x=c("site","nom"),by.y=c("site","point"))

PartSelG$month=month(PartSelG$date_debut)
PartSelG$day=substr(PartSelG$date_debut,1,2)

PartSelG$yday=(PartSelG$month-1)*30+as.numeric(PartSelG$day)

fwrite(SiteLoc,"SL_pourQGIS.csv",sep=";")
fwrite(PartSelG,"PartSelG.csv",sep=";")
