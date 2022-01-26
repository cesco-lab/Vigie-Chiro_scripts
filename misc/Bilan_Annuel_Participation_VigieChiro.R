library(data.table)
library(ggplot2)
library(reshape2)
library(raster)
#library(rgdal)
library(rgeos)
library(lubridate)

Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv",encoding="UTF-8")
#SiteLoc=fread("C:/Users/yvesb/Documentswww/sites_localites.txt",encoding="UTF-8")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
SelDep=F
#Dep=c("24","33","40","47","64")
Sys.time()
FranceD= shapefile("C:/Users/yvesb/Documents/SIG/Limite_administrative/France_dep_L93.shp")
Sys.time()
StartYear=2011
EndYear=2021


if(SelDep)
{
  FranceD=subset(FranceD,FranceD$D?pARTEM0 %in% Dep)
}

SiteLoc0=SiteLoc
coordinates(SiteLoc) <- c("longitude", "latitude")
proj4string(SiteLoc) <- CRS("+init=epsg:4326") # WGS 84
FranceWGS84=spTransform(FranceD,CRS(proj4string(SiteLoc)))
#SiteLoc=crop(SiteLoc,extent(FranceWGS84))
#SiteLoc=gIntersection(SiteLoc,FranceWGS84,byid=T)
SiteLoc=raster::intersect(SiteLoc,FranceWGS84)






Particip=as.data.frame(Particip)
Annee=substr(Particip$date_debut,7,10)

#dirty way to get the protocol type 
Protocole=substr(Particip$site,13,nchar(Particip$site)-7)
Proto3=c("Routier","Routier","Routier","Pedestre","Point Fixe") 
Proto33=as.numeric(as.factor(Protocole))
Proto333=Proto3[Proto33]
Particip$Proto333=Proto333
Particip$Annee=Annee

Particip=subset(Particip,Particip$site %in% SiteLoc$site)


table(Particip$Annee)
ListSiteAnnee=aggregate(Particip$participation,
                        by=list(Particip$Annee,Particip$site),FUN=length)
NbSiteAnnee=aggregate(ListSiteAnnee$Group.2,
                      by=list(ListSiteAnnee$Group.1),FUN=length)
NbPartAnneeProt=dcast(data=Particip,Annee~Proto333,fun.aggregate=length)

#suppress current year
NbPartAnneePProt=subset(NbPartAnneeProt
                        ,NbPartAnneeProt$Annee<as.numeric(substr(Sys.time(),1,4)))

dd=reshape2::melt(NbPartAnneePProt, id=c("Annee"))
colnames(dd)[2]="Protocole"
ggplot(dd) + 
  geom_line(aes(x=as.numeric(Annee), y=value
                , colour=Protocole),size=1) +  
  scale_colour_manual(values=c("red","green","blue"))+
  scale_x_continuous(breaks = seq(min(as.numeric(Annee))
                                  ,max(as.numeric(Annee)), 1)) +
    xlab("Annee")+ylab("nb participations")+
  scale_y_log10()


Mois=substr(Particip$date_debut,4,5)
NbPartMoisProt=dcast(data=Particip,Mois~Proto333,fun.aggregate=length)
dd=reshape2::melt(NbPartMoisProt, id=c("Mois"))
colnames(dd)[2]="Protocole"
ggplot(dd) + 
  geom_line(aes(x=as.numeric(Mois), y=value
                , colour=Protocole),size=1) +  
  scale_colour_manual(values=c("red","green","blue"))+
  scale_x_continuous(breaks = seq(min(as.numeric(Mois))
                                  ,max(as.numeric(Mois)), 1)) +
  xlab("Mois")+ylab("nb participations")+
  scale_y_log10()


SiteProt=aggregate(SiteLoc$localite,by=list(SiteLoc$protocole),FUN=length)

SiteProtP=aggregate(Particip$Proto333,by=c(list(Particip$Proto333),list(Particip$site),list(Particip$point)),FUN=length)
SiteProtP2=aggregate(SiteProtP$Group.1,by=list(SiteProtP$Group.1),length)

SiteProt$Group.1[1]="PEDESTRE"

barplot(SiteProt$x,names.arg=SiteProt$Group.1,main="nb localites"
        ,hor=T,las=2)

DateDeb=as_datetime(Particip$date_debut,format="%d/%m/%Y %H:%M")
DateFin=as_datetime(Particip$date_fin,format="%d/%m/%Y %H:%M")
Dur=floor((as.numeric(DateFin)-as.numeric(DateDeb))/24/3600+0.7)
Dur=ifelse(Dur>30,1,Dur)
Particip$Dur=ifelse(Dur<0,1,Dur)
Particip$Mois=substr(Particip$date_debut,4,5)

ParticipPF=subset(Particip,Particip$Proto333=="Point Fixe")
ParticipPF=subset(ParticipPF,!is.na(ParticipPF$Dur))

NNights=aggregate(ParticipPF$Dur,by=list(ParticipPF$Mois),sum)

ggplot(NNights)+
  geom_line(aes(x=as.numeric(Group.1), y=x))

barplot(NNights$x,names.arg=NNights$Group.1,xlab="Month",ylab="n nights")

YNights=aggregate(ParticipPF$Dur,by=list(ParticipPF$Annee),sum)
YNights=subset(YNights,YNights$Group.1>=StartYear)
YNights=subset(YNights,YNights$Group.1<=EndYear)

barplot(YNights$x,names.arg=YNights$Group.1,xlab="Year",ylab="n nights")

UProtP=aggregate(Particip$Proto333,by=c(list(Particip$Proto333),list(Particip$observateur)),FUN=length)
UProtP2=aggregate(UProtP$Group.1,by=list(UProtP$Group.1),length)
UProtP2
