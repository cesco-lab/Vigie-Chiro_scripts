library(data.table)
library(FNN)
library(sp)

ETV_filtree=fread("ETV_filtree.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
Sites_localites=fread("C:/wamp64/www/sites_localites.txt")
LongMin=-5.313758
LongMax=10.517564
LatMin=40.395428 
LatMax=52.001544



Particip$Date1=as.Date(substr(Particip$date_debut,1,10),format="%d/%m/%Y")
Particip$JJulien=yday(Particip$Date1)
Particip$JJ228=Particip$JJulien-228+365*(Particip$JJulien<229)
Particip=subset(Particip,Particip$nb_don>0)

Particip$JJ228_scale=scale(Particip$JJ228)
Sites_localites$longitude_scale=scale(Sites_localites$longitude)
Sites_localites$latitude_scale=scale(Sites_localites$latitude)
Sites_localites=subset(Sites_localites,(Sites_localites$longitude>LongMin)
                       &(Sites_localites$longitude<LongMax)
                       &(Sites_localites$latitude>LatMin)
                       &(Sites_localites$latitude<LatMax))


PartETV=subset(Particip,Particip$participation %in% ETV_filtree$participation)
ObsETV=levels(as.factor(PartETV$idobservateur))

Sites_localitesRP=subset(Sites_localites,Sites_localites$protocole!="POINT_FIXE")
#trondetail=tstrsplit(Sites_localitesRP$nom," ")
ParticipSLRP=merge(Sites_localitesRP,Particip,by="site",allow.cartesian=TRUE)

ParticipSLRP_v=subset(ParticipSLRP,ParticipSLRP$participation %in% PartETV$participation)
ParticipSLRP_av=subset(ParticipSLRP,!ParticipSLRP$participation %in% PartETV$participation)


ARajouter=vector()
for (i in 1:10)
{
  

Coord1=cbind(ParticipSLRP_v$longitude_scale,ParticipSLRP_v$latitude_scale
             ,ParticipSLRP_v$JJ228_scale)


Coord2=cbind(ParticipSLRP_av$longitude_scale,ParticipSLRP_av$latitude_scale
             ,ParticipSLRP_av$JJ228_scale)



g = get.knnx(coordinates(Coord1), coordinates(Coord2),k=1)

AuteursV=(ParticipSLRP_av$observateur.y %in% ParticipSLRP_v$observateur.y)

Dist=g[[2]]-AuteursV
LePlusLoin=which.max(Dist)
PartNew=(ParticipSLRP_av$participation[LePlusLoin])
print(PartNew)
ARajouter=c(ARajouter,PartNew)

PartSel=subset(ParticipSLRP,ParticipSLRP$participation==PartNew)
ParticipSLRP_v=rbind(ParticipSLRP_v,PartSel)

ParticipSLRP_av=subset(ParticipSLRP_av
                       ,ParticipSLRP_av$participation!=PartNew)
}

Sites_localitesPF=subset(Sites_localites,Sites_localites$protocole=="POINT_FIXE")
#trondetail=tstrsplit(Sites_localitesRP$nom," ")
ParticipSLPF=merge(Sites_localitesPF,Particip,by.x=c("site","nom")
                   ,by.y=c("site","point"),allow.cartesian=TRUE)

ParticipSLPF_v=subset(ParticipSLPF,ParticipSLPF$participation %in% PartETV$participation)
ParticipSLPF_av=subset(ParticipSLPF,!ParticipSLPF$participation %in% PartETV$participation)


for (i in 1:10)
{
  
  
  Coord1=cbind(ParticipSLPF_v$longitude_scale,ParticipSLPF_v$latitude_scale
               ,ParticipSLPF_v$JJ228_scale)
  
  
  Coord2=cbind(ParticipSLPF_av$longitude_scale,ParticipSLPF_av$latitude_scale
               ,ParticipSLPF_av$JJ228_scale)
  
  
  
  g = get.knnx(coordinates(Coord1), coordinates(Coord2),k=1)

  AuteursV=(ParticipSLPF_av$observateur.y %in% ParticipSLPF_v$observateur.y)
  
  Dist=g[[2]]-AuteursV
  LePlusLoin=which.max(Dist)
  PartNew=(ParticipSLPF_av$participation[LePlusLoin])
  print(PartNew)
  ARajouter=c(ARajouter,PartNew)
  
  PartSel=subset(ParticipSLPF,ParticipSLPF$participation==PartNew)
  ParticipSLPF_v=rbind(ParticipSLPF_v,PartSel)
  
  ParticipSLPF_av=subset(ParticipSLPF_av
                         ,ParticipSLPF_av$participation!=PartNew)
}


fwrite(as.data.frame(ARajouter),"ARajouter.csv",col.names=F)

#test=subset(Particip,Particip$participation=="5a076e96fe80cc000db43afb")
#test=subset(Sites_localitesP,Sites_localitesP$id_site=="564c90b7eea470000e1d19a3")

