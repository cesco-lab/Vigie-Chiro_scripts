library(data.table)
Particip=fread("C:/wamp64/www/p_export.csv")
LogSPS=fread("C:/wamp64/www/logsps.txt")
ListeTAZ=fread("C:/wamp64/www/listetaz.txt",h=F)
Protpriority=c("Vigiechiro - Point Fi","Vigie-chiro - Routier","Vigiechiro - Pédestr")
Deppriority=c(34,30,48,11,66,12,81,82,46,32,31,9,65,75,77,78,91,92,93,94,95)


TAZ_Archives=gsub(".tar.gz","",ListeTAZ$V1)

LogSPS$participation=gsub("vigiechiro/vigiechiro-prod-datastore/","",LogSPS$`Directory or file in ""/sps/mnhn""`)

Particip$TAZ=(Particip$participation %in% TAZ_Archives)
Particip$SPS=(Particip$participation %in% LogSPS$participation)

Particip$protocole=substr(Particip$site,1,21)
PP=match(Particip$protocole,Protpriority)

#location priority
Particip$Dep=as.numeric(substr(Particip$site,25,26))
DP=match(Particip$Dep,Deppriority)
DP[is.na(DP)]=99
Particip$priority=PP*100+DP
barplot(table(Particip$priority),las=2)

#order by priority then author
Particip_P=Particip[order(Particip$priority,Particip$observateur),]
Particip_P$Pnum=c(1:nrow(Particip_P))
Particip_P=Particip_P[order(Particip_P$TAZ),]
fwrite(Particip_P,"Particip_P.csv",sep=";")

