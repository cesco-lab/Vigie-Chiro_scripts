library(data.table)
library(ggplot2)

Particip=fread("C:/wamp64/www/p_export.csv")

Particip$Annee=substr(Particip$date_debut,7,10)
barplot(table(Particip$Annee),las=2)

ParticipP=subset(Particip,Particip$point!="")

PointsManquants=subset(Particip,(Particip$point=="")
                       &(grepl("Fixe",Particip$site)))

ParticipP$SiteLocalite=paste(ParticipP$site,ParticipP$point)

ParticipAgg1=aggregate(ParticipP$participation,by=c(list(ParticipP$SiteLocalite)
                                      ,list(ParticipP$Annee)),FUN=length)
ParticipAgg2=aggregate(ParticipAgg1$Group.2,by=list(ParticipAgg1$Group.1)
                       ,FUN=length)

ParticipRepA=subset(ParticipAgg2,ParticipAgg2$x>1)
barplot(table(ParticipRepA$x))

ParticipPR=subset(ParticipP,ParticipP$SiteLocalite %in% ParticipRepA$Group.1)

ParticipPR_A1=aggregate(as.numeric(ParticipPR$Annee)
                        ,by=list(ParticipPR$SiteLocalite)
                        ,FUN=min)

ParticipPR_ALast=aggregate(as.numeric(ParticipPR$Annee)
                           ,by=list(ParticipPR$SiteLocalite)
                        ,FUN=max)

ggplot(ParticipPR_A1, aes(x=x, y=Dur )) +
  geom_bin2d() +
  theme_bw()


ggplot(ParticipPR_A1, aes(x=x, y=ALast )) +
  geom_bin2d() +
  theme_bw()

