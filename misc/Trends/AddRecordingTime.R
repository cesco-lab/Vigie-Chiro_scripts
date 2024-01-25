library(data.table)
library(lubridate)

#DataTot=fread("C:/Users/yvesb/Downloads/DataTot.csv")
DurEnr=fread("C:/Users/yvesb/Downloads/DurEnr.csv")
DataTrends=fread("C:/Users/yvesb/Downloads/sorting_custom.csv")

head(DataTrends$site)
#head(DataTot$numero_carre)

#DataTot$site=paste0("Vigiechiro - Point Fixe-",DataTot$numero_carre)

DataTrends$siteloc=paste(DataTrends$site,DataTrends$Tron)

#DataTot$siteloc=paste(DataTot$site,DataTot$point)

DataTrendsPF=subset(DataTrends,DataTrends$protocole=="POINT_FIXE")

DurEnr$day=ymd(DurEnr$Group.2)
DurEnr$julian=yday(DurEnr$day)
DurEnr$year=as.integer(substr(DurEnr$Group.2,1,4))
  


Testmerge=merge(DataTrendsPF,DurEnr,by.x=c("participation","julian","year"),by.y=c("Group.1","julian","year"))

match1=paste(DataTrends$participation,DataTrends$julian,DataTrends$year)  
match2=paste(DurEnr$Group.1,DurEnr$julian,DurEnr$year)

test=match(match1,match2)

DataTrends$temps_enr=ifelse(is.na(test),DataTrends$temps_enr,DurEnr$duree_enregistrement[test]*3600)
DataTrends$nuit_complete=ifelse(is.na(test),F,DurEnr$nuit_complete[test])
boxplot(DataTrends$temps_enr~DataTrends$protocole)
table(DataTrends$nuit_complete,DataTrends$protocole)

DataTrends=subset(DataTrends,(DataTrends$protocole!="POINT_FIXE")|(DataTrends$nuit_complete))

table(DataTrends$nuit_complete,DataTrends$protocole)
boxplot(DataTrends$temps_enr~DataTrends$protocole)

fwrite(DataTrends,"DataTrends.csv",sep=";")
