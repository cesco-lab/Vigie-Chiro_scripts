library(data.table)

#DataTot=fread("C:/Users/yvesb/Documents/www/DataTot.csv")
Particip=fread("p_export_forLinux.csv")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
Users=fread("C:/Users/yvesb/Documents/www/utilisateurs_raw.csv")
AnneeFirst=2006

testU=match(Particip$idobservateur,Users$'_id')

Open=Users$donnees_publiques[testU]
print(table(Open))
print(table(Open)[1]/(table(Open)[1]+table(Open)[2]))

Particip$aggregat_reseau=Users$organisation[testU]
table(Particip$aggregat_reseau)

Particip=subset(Particip,Open)

YearNow=as.integer(substr(Sys.Date(),1,4))
Annee=c(AnneeFirst:YearNow)

LongA=aggregate(SiteLoc$longitude,by=list(SiteLoc$id_site),mean)
LatA=aggregate(SiteLoc$latitude,by=list(SiteLoc$id_site),mean)


testS=match(Particip$idsite,LongA$Group.1)
summary(is.na(testS))

Particip$longitude=LongA$x[testS]
Particip$latitude=LatA$x[testS]

testS2=match(Particip$idsite,SiteLoc$id_site)
table(SiteLoc$protocole)

Particip$protocole=SiteLoc$protocole[testS2]

Particip=subset(Particip,!is.na(testS))
Particip$protocole[Particip$protocole=="CARRE"]="PEDESTRE"
table(Particip$protocole)


Particip$year=substr(Particip$date_debut,1,4)
print(table(Particip$year))
ParticipFilterDate=subset(Particip,Particip$year %in% Annee)
NbLost=nrow(Particip)-nrow(ParticipFilterDate)
print(paste(NbLost,"participations dont la date est hors des clous"))


ParticipFilterDate$participation_date=as.Date(ParticipFilterDate$date_debut)


PourDashboard=subset(ParticipFilterDate,select=c("latitude","longitude","participation_date","idobservateur"
                                                 ,"participation","protocole","site"
                                                 ,"aggregat_reseau"))

names(PourDashboard)=c("latitude","longitude","participation_date","user_id"
                       ,"participation_id","protocole","aggregat_site","aggregat_reseau")

DateColle=gsub("-","",Sys.Date())

fwrite(PourDashboard,paste0("Vigie-Chiro_",DateColle,".csv"),sep=";")
