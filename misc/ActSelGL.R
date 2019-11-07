library(data.table)
FAct="D:/VigieChiro/Raw/SpNuit2_sunrise7200-180050_DataLP_PF_exportTot.csv"
#FAct="D:/VigieChiro/Raw/SpNuit2_90_DataLP_PF_exportTot.csv"
coefs=fread("coefs190825.csv")
#SelYDay=229
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")



Act=fread(FAct)
ActMoy=aggregate(Act$nb_contacts,by=list(Act$espece),mean)

#exfiltrer les gites?

Act=merge(Act,ActMoy,by.="espece",by.y="Group.1")
Act$actstd=Act$nb_contacts/Act$x
Act=merge(Act,coefs,by.x="espece",by.y="NOM_VALIDE")
Act$scoresp=Act$actstd*Act$coef
Act$mois=as.numeric(substr(Act$Nuit,6,7))
Act$day=as.numeric(substr(Act$Nuit,9,10))
Act$yday=(Act$mois-1)*30+Act$day

ScoreTot=aggregate(Act$scoresp,by=c(list(Act$participation)
                                    ,list(Act$Nuit)
                                    ,list(Act$num_micro)),FUN=sum)

Act=merge(Act,ScoreTot,by.x=c("participation","Nuit","num_micro")
          ,by.y=c("Group.1","Group.2","Group.3"))

test=subset(Act,Act$participation=="5d36f4865d2065001af5ddbd")

#ActSel=subset(Act,Act$yday>SelYDay-30)
#ActSel=subset(ActSel,ActSel$yday<SelYDay+15)
ActSel=Act
ActSel[which.max(ActSel$x.y),]
ActSel=ActSel[order(ActSel$x.y,decreasing=T),]
fwrite(ActSel,"ActSel.csv")

Geoloc=merge(SiteLoc,Particip,by.x=c("site","nom"),by.y=c("site","point"))
Geoloc=subset(Geoloc,Geoloc$protocole=="POINT_FIXE")

ActSelGL=merge(ActSel,Geoloc,by="participation")
fwrite(ActSelGL,paste0("ActSelGL_",basename(FAct)))

       