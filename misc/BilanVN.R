library(data.table)

Utilisateurs=fread("C:/Users/yvesb/Documents/www/utilisateurs.txt")
Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
Years=c(2006:2022)
SpNuit=fread("C:/Users/yvesb/Documents/www/SpNuit_2DI_0_DataLP_PF_exportTot.csv")
SpTron=fread("C:/Users/yvesb/Downloads/DataRP_SpTron_woS_0.csv")

Utilisateurs$nom_complet=paste(Utilisateurs$prenom,Utilisateurs$nom)

Ushort=subset(Utilisateurs,select=c("identifiant","nom_complet","pseudo","email"
                                    ,"date inscription"))
              
Ushort$nom_observatoire="Vigie-Chiro&SON"
Ushort$public="naturalistes"
Ushort$taxon="animaux"

#test=match(Utilisateurs$identifiant,Particip$idobservateur)

Pshort=subset(Particip,select=c("idobservateur","participation","date_debut","idsite"))

Year=substr(Pshort$date_debut,7,10)
table(Year)

Pshort=subset(Pshort,Year %in% Years)

CoordM=aggregate(cbind(SiteLoc$longitude,SiteLoc$latitude),by=list(SiteLoc$id_site),mean)

testSL=match(Pshort$idsite,CoordM$Group.1)
summary(is.na(testSL))
Pshort$longitude=CoordM$V1[testSL]
Pshort$latitude=CoordM$V2[testSL]

#get RP diversity
SpAnimals=subset(SpTron,SpTron$espece!="noise")
ListSp=unique(SpAnimals,by=c("participation","espece"))
NSpRP=aggregate(ListSp$espece,by=list(ListSp$participation),length)
hist(NSpRP$x)

#get PF diversity
SpPFa=subset(SpNuit,SpNuit$espece!="noise")
SpPFa=subset(SpPFa,SpPFa$nb_contacts_nd>0)
ListSpPF=unique(SpPFa,by=c("participation","espece"))
NSpPF=aggregate(ListSpPF$espece,by=list(ListSpPF$participation),length)
hist(NSpPF$x)

NSpAll=rbind(NSpRP,NSpPF)
hist(NSpAll$x)

testSp=match(Pshort$participation,NSpAll$Group.1)
summary(is.na(testSp))

Pshort$diversite=NSpAll$x[testSp]

testU=match(Pshort$idobservateur,Ushort$identifiant)
summary(is.na(testU))
Pshort$nom=Ushort$nom_complet[testU]
Pshort$pseudo=Ushort$pseudo[testU]
Pshort$email=Ushort$email[testU]
Pshort$date_inscription=Ushort$`date inscription`[testU]

summary(Pshort)

fwrite(Pshort,"BilanVigieChiro_pourVN.csv",sep=";")
