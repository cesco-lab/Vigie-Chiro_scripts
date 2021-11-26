library(data.table)
library(randomForest)

SpNuit=fread("C:/wamp64/www/SpNuit2_0_DataLP_PF_exportTot.csv")
SpeciesList=fread("SpeciesList.csv")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.csv")
ReducForTest=T
TestSize=20


BatList=subset(SpeciesList,SpeciesList$Group=="bat")

BatNuit=subset(SpNuit,SpNuit$espece %in% BatList$Esp)


BatActTot=aggregate(BatNuit$nb_contacts,by=c(list(BatNuit$participation)
                                             ,list(BatNuit$Nuit)
                                             ,list(BatNuit$num_micro))
                    ,sum)

Date1=as.Date(BatActTot$Group.2)


SpFDate=yday(Date1)
BatActTot$SpCDate=cos(SpFDate/365*2*pi)
BatActTot$SpSDate=sin(SpFDate/365*2*pi)
#plot(BatActTot$SpCDate,BatActTot$SpSDate)

BatListeff=unique(BatNuit$espece)

for (i in 1:length(BatListeff))
{
  print(BatListeff[i])
  iNuit=subset(BatNuit,BatNuit$espece==BatListeff[i])
  Corri=match(paste(BatActTot$Group.1,BatActTot$Group.2,BatActTot$Group.3)
              ,paste(iNuit$participation,iNuit$Nuit,iNuit$num_micro)
              )
  NbCi=iNuit$nb_contacts[Corri]
  NbCi[is.na(NbCi)]=0
  BatActTot=cbind(BatActTot,NbCi)
  names(BatActTot)[ncol(BatActTot)]=paste0(BatListeff[i],"_nc")
  Propi=NbCi/BatActTot$x
  BatActTot=cbind(BatActTot,Propi)
  names(BatActTot)[ncol(BatActTot)]=paste0(BatListeff[i],"_prop")
  Confi=iNuit$score_max[Corri]
  Confi[is.na(Confi)]=0
  BatActTot=cbind(BatActTot,Confi)
  names(BatActTot)[ncol(BatActTot)]=paste0(BatListeff[i],"_conf")
}

PSL=merge(Particip,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
PSL=subset(PSL,grepl("Fixe",PSL$site))

test=match(BatActTot$Group.1,PSL$participation)

BatActTot$SpGite=PSL$SpGite[test]
BatActTot$site=PSL$site[test]
BatActTot$site_loc=paste(PSL$site[test],PSL$nom[test])
BatActTot=subset(BatActTot,!is.na(BatActTot$SpGite))

if(ReducForTest)
{
  BatActTot_Save=BatActTot
  BatActTot=BatActTot[1:TestSize,]
}

#BatActTot=BatActTot_Save

Predictors=subset(BatActTot
                  ,select=subset(names(BatActTot)
                          ,!names(BatActTot) %in% c("Group.1","Group.2"
                                                    ,"Group.3","x","SpGite"
                                                    ,"site","site_loc")))

ClassifGite=randomForest(x=Predictors
                         ,BatActTot$SpGite
                         ,strata=BatActTot$site_loc)

BatActTot$PredGite=ClassifGite$predicted

#compare known and predicted
SLknown=aggregate(BatActTot$SpGite,by=list(BatActTot$site_loc),FUN=mean)
SLpred=aggregate(BatActTot$PredGite,by=list(BatActTot$site_loc),FUN=mean)

SLcomp=cbind(SLknown,Pred=SLpred$x)

