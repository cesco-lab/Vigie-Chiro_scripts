library(data.table)
library(Hmisc)

Tag="1017"
Data=fread("./VigieChiro/Exports/_Veolia_1017_SL.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
MoisPeak=c("06","07","08","09")
SLpred="./VigieChiro/gbifData/SL"
SpeciesList=fread("SpeciesList.csv")
Referentiel=fread("./VigieChiro/Referentiels/refPF_Total_2020-04-10.csv")
ReferentielReg=fread("./VigieChiro/Referentiels/refPF_Ile-de-France_2020-04-10.csv")
DirPred="./VigieChiro/ModPred/Veolia/3107"
HabitatTraits=fread("./VigieChiro/Traits/HabitatTraitsSpecies.csv")
RefIndicateurs=fread("RefIndicators2021-01-19.csv")
#SpeciesToExclude="Rhihip"

f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

alogit <- function(x) 
{
  exp(x)/(1+exp(x))
}

find_modes<- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] > x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}

find_thresholds<- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] < x[i-1]) & (x[i] < x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}

DataExcluded=subset(Data,(Data$espece %in% SpeciesToExclude))
print(max(DataExcluded$probabilite))
Data=subset(Data,!(Data$espece %in% SpeciesToExclude))


SLfiles=list.files(SLpred,full.names=T)

Particip$Mois=substr(Particip$date_debut,4,5)
Data$Mois=substr(Data$donnee,nchar(Data$donnee)-14,nchar(Data$donnee)-13)
table(Data$Mois)
Data$Annee=substr(Data$donnee,nchar(Data$donnee)-18,nchar(Data$donnee)-15)
table(Data$Annee)
Data$Annee_Mois=paste(Data$Annee,Data$Mois,sep="_")
Data$Dur=Data$temps_fin-Data$temps_debut



BatData=subset(Data,Data$GroupFR=="Chauve-souris")
table(BatData$espece)
Pdata=subset(Particip,Particip$participation %in% Data$participation)
SLd=subset(SiteLoc,(SiteLoc$site==Pdata$site[1])&
             (SiteLoc$nom==Pdata$point[1]))
NbContacts=aggregate(BatData$participation,by=list(BatData$espece),length)
ScoreMoyen=aggregate(BatData$probabilite,by=list(BatData$espece),mean)
ScoreMax=aggregate(BatData$probabilite,by=list(BatData$espece),max)

Bat1sec=subset(BatData,BatData$Dur>1)
ScoreMoyen1sec=aggregate(Bat1sec$probabilite,by=list(Bat1sec$espece),mean)

DurMoyenne=aggregate(BatData$Dur,by=list(BatData$espece),mean)

#examen de la durée pour détecter d'éventuels problèmes
PipData=subset(BatData,substr(BatData$espece,1,3)=="Pip")
boxplot(PipData$Dur~PipData$Annee_Mois,las=2)



Thres=vector()
BatDataFiltered=BatData[0,]
for (i in 1:length(NbContacts$Group.1))
{
  Datai=subset(BatData,BatData$espece==NbContacts$Group.1[i])
  Spi=subset(SpeciesList,SpeciesList$Esp==NbContacts$Group.1[i])
  SLi=subset(SLfiles,grepl(Spi$`Scientific name`[1]
                           ,basename(SLfiles)))
  if(length(SLi)>0)
  {
    SLpredi=fread(SLi[1])
    id=find.matches(cbind(SLd$longitude,SLd$latitude)
                    ,cbind(SLpredi$Group.1,SLpredi$Group.2)
                    ,maxmatch = 1,tol=c(1,1))
    SLpredid=SLpredi[id$matches[1],]
    print(paste(NbContacts$Group.1[i],SLpredid$pred))
    
  }else{
    #stop("especes manquantes")
    print(paste("espece manquante :",NbContacts$Group.1[i]))
  }
  if(SLpredid$pred>0.2)
  {
    
    if(nrow(Datai)>1){
      
      Dflow=density(Datai$probabilite)
      
      plot(Dflow,main=NbContacts$Group.1[i])
      modes=density(Datai$probabilite)$x[find_modes(Dflow$y)]
      thresholds=density(Datai$probabilite)$x[find_thresholds(Dflow$y)]
      modesTRUE=subset(modes,modes>0.5)
      modesFALSE=subset(modes,modes<=0.5)
      if(length(modesTRUE)>0)
      {
        if(length(modesFALSE)>0)
        {
          thresSel=subset(thresholds,(thresholds>max(modesFALSE))&
                            (thresholds<min(modesTRUE)))
          Thres=c(Thres,thresSel[1])
        }else{
          Thres=c(Thres,0)
        }
      }else{
        Thres=c(Thres,1)
      }
      
    }else{
      Thres=c(Thres,0.5)
    }
  }else{
    Thres=c(Thres,1)
  }
  DataiFiltered=subset(Datai,Datai$probabilite>Thres[i])
  #print(NbContacts$Group.1[i])
  #print(head(DataiFiltered$donnee))
  #print(head(DataiFiltered$temps_debut))
  #print(head(DataiFiltered$temps_fin))
  BatDataFiltered=rbind(BatDataFiltered,DataiFiltered)
}
table(BatDataFiltered$espece)
table(BatDataFiltered$espece,BatDataFiltered$Mois)

fwrite(BatDataFiltered,paste0("BatDataFiltered_",Tag,".csv"),sep=";")

PipData=subset(BatDataFiltered,substr(BatDataFiltered$espece,1,3)=="Pip")
SpData=subset(BatDataFiltered,BatDataFiltered$espece=="MyoGT")
SpData$donnee
SpData

boxplot(PipData$Dur~PipData$Annee_Mois,las=2)
m1=lm(PipData$Dur~as.numeric(as.factor(PipData$Annee_Mois)))
summary(m1)
jpeg(paste(Tag,"_DureeSequencesPip.jpg"))
     boxplot(PipData$Dur~PipData$Annee_Mois,las=2)
     dev.off()


NbContacts=aggregate(BatDataFiltered$participation,by=list(BatDataFiltered$espece),length)
ScoreMoyen=aggregate(BatDataFiltered$probabilite,by=list(BatDataFiltered$espece),mean)

BatPartData=aggregate(BatDataFiltered$donnee,by=c(list(BatDataFiltered$espece)
                                          ,list(BatDataFiltered$participation))
                      ,length)
BatNPart=aggregate(BatPartData$x,by=list(BatPartData$Group.1),length)

BatPeak=subset(BatDataFiltered,BatDataFiltered$Mois %in% MoisPeak)
NbContactsPeak=aggregate(BatPeak$participation
                         ,by=list(BatPeak$espece),length)
Ppeak=subset(Pdata,Pdata$Mois %in% MoisPeak)

BatSummary1=cbind(NbContacts,Risque_erreur_moyen=round(1-ScoreMoyen$x,2))
BatSummary1$Activite_Moyenne=round(BatSummary1$x/nrow(Pdata),3)
BatSummary1$Occurrence=round(BatNPart$x/nrow(Pdata),3)

BatPartDataP=aggregate(BatPeak$donnee,by=c(list(BatPeak$espece)
                                                  ,list(BatPeak$participation))
                      ,length)
BatNPartP=aggregate(BatPartDataP$x,by=list(BatPartDataP$Group.1),length)


NbContactsPeak$Activite_Moyenne_Pic=round(NbContactsPeak$x/nrow(Ppeak),3)
NbContactsPeak$Occurrence_Pic=round(BatNPartP$x/nrow(Ppeak),3)

BatSummary2=merge(BatSummary1,NbContactsPeak,by="Group.1",all.x=T)
BatSummary2$x.y=NULL
BatSummary2[BatSummary2==""]=0
names(BatSummary2)[names(BatSummary2)=="x.x"]="Nb_enregistrements"
CorrRef=match(BatSummary2$Group.1,Referentiel$Espece)
BatSummary2$Activite_FR=round(Referentiel$MoyG[CorrRef],2)
CorrRefR=match(BatSummary2$Group.1,ReferentielReg$Espece)
BatSummary2$Activite_Regionale=round(ReferentielReg$MoyG[CorrRefR],2)

#recup des predictions sur le point
CoordPt=cbind(SLd$longitude,SLd$latitude)
ListPred=list.files(DirPred,full.names=T)
BatSummary2$Activite_Predite=rep(0,nrow(BatSummary2))
for (j in 1:nrow(BatSummary2))
{
  Spj=BatSummary1$Group.1[j]
  Listj=subset(ListPred,grepl(Spj,basename(ListPred)))
  Predj=fread(Listj[1])
  test=find.matches(CoordPt,cbind(Predj$Group.1,Predj$Group.2),tol=c(0.1,0.1)
                    ,maxmatch=1)
  BatSummary2$Activite_Predite[j]=round(10^(Predj$pred[test$matches[1]])-1,2)
}


CorrSp=match(BatSummary2$Group.1,SpeciesList$Esp)
BatSummary2$Nom_francais=SpeciesList$NomFR[CorrSp]
BatSummary2$Nom_scientifique=SpeciesList$`Scientific name`[CorrSp]

fwrite(BatSummary2,paste0("BatSummary_",Tag,".csv"),sep=";")

#production d'indicateurs
TempsEnregistrement2=sapply(BatDataFiltered$donnee,FUN=f2pPF) #long à tourner (3e5 données/min)
test=subset(BatDataFiltered,is.na(TempsEnregistrement2))
Sys.time()
pourDateNuit=TempsEnregistrement2-12*3600 #bricolage-décalage de 12 heures pour ramener à la date du début de nuit
Sys.time()
DateNuit=as.Date.POSIXct(pourDateNuit) # date of the beginning of the night

DataBat=aggregate(BatDataFiltered$donnee,by=c(list(BatDataFiltered$espece)
                                              ,list(DateNuit)),length)

DataTraits=merge(DataBat,HabitatTraits,by.x="Group.1",by.y="Species")
DataTraits$ActStand=DataTraits$x/DataTraits$ActMoy
summary(DataTraits$ActStand)

NuitActTot=aggregate(DataTraits$ActStand,by=c(list(DataTraits$Group.2)
                                  ),sum)
NuitsumSSI=aggregate(DataTraits$ActStand*DataTraits$SSI
                     ,by=c(list(DataTraits$Group.2)
                     ),sum)
NuitCSI=NuitsumSSI$x/NuitActTot$x
plot(log(NuitCSI-0.5))

test=subset(DataTraits,DataTraits$Group.2=="2020-09-18")


DataTraits$wAlan=pmax(0,-DataTraits$RepAlan)
summary(DataTraits$wAlan)
DataTraits$wWood=pmax(0,DataTraits$RepWood)
DataTraits$wWater=pmax(0,DataTraits$RepWater)
NuitAlan=aggregate(DataTraits$ActStand*DataTraits$wAlan
                   ,by=c(list(DataTraits$Group.2)
                   ),sum)
plot(log(NuitAlan$x+0.00001))

NuitWood=aggregate(DataTraits$ActStand*DataTraits$wWood
                   ,by=c(list(DataTraits$Group.2)
                   ),sum)
plot(log(NuitWood$x+0.001))

NuitWater=aggregate(DataTraits$ActStand*DataTraits$wWater
                    ,by=c(list(DataTraits$Group.2)
                    ),sum)
plot(log(NuitWater$x+0.001))

CSIstd=vector()
Alanstd=vector()
Woodstd=vector()
Waterstd=vector()
for (k in 1:nrow(NuitActTot))
{
  CSIstd=c(CSIstd,weighted.mean(log(NuitCSI[max(k-2,1):
                                              (min(nrow(NuitActTot),k+2))]
                                    -0.5)
                       ,w=NuitActTot$x[max(k-2,1):
                                         (min(nrow(NuitActTot),k+2))]))
  Alanstd=c(Alanstd,weighted.mean(log(NuitAlan$x[max(k-2,1):
                                              (min(nrow(NuitActTot),k+2))]
                                      +0.00001)
                                ,w=NuitActTot$x[max(k-2,1):
                                                  (min(nrow(NuitActTot),k+2))]))
  Woodstd=c(Woodstd,weighted.mean(log(NuitWood$x[max(k-2,1):
                                                   (min(nrow(NuitActTot),k+2))]
                                      +0.001)
                                  ,w=NuitActTot$x[max(k-2,1):
                                                    (min(nrow(NuitActTot),k+2))]))
  Waterstd=c(Waterstd,weighted.mean(log(NuitWater$x[max(k-2,1):
                                                   (min(nrow(NuitActTot),k+2))]
                                      +0.001)
                                  ,w=NuitActTot$x[max(k-2,1):
                                                    (min(nrow(NuitActTot),k+2))]))
  

      
}
plot(CSIstd)
plot(Alanstd)
plot(Woodstd)
plot(Waterstd)

Indicateur_Spec=alogit((CSIstd-RefIndicateurs$Mean[1])/
                          RefIndicateurs$sd[1])*20
plot(Indicateur_Spec)
Indicateur_Lum=alogit((Alanstd-RefIndicateurs$Mean[2])/
                         RefIndicateurs$sd[2])*20
plot(Indicateur_Lum)
Indicateur_Arb=alogit((Woodstd-RefIndicateurs$Mean[3])/
                        RefIndicateurs$sd[3])*20
plot(Indicateur_Arb)
Indicateur_Eau=alogit((Waterstd-RefIndicateurs$Mean[4])/
                        RefIndicateurs$sd[4])*20
plot(Indicateur_Eau)

Indicateur_Dates=data.frame(Nuit=NuitActTot$Group.1
                            ,Indicateur_Spec,Indicateur_Lum
                            ,Indicateur_Arb,Indicateur_Eau)
fwrite(Indicateur_Dates,paste0("Indicateur_Dates",Tag,".csv"),sep=";")

Indicateur_Pic=subset(Indicateur_Dates,substr(Indicateur_Dates$Nuit,6,7)
                      %in% MoisPeak)


MeanSpec=mean(Indicateur_Pic$Indicateur_Spec)
MeanLum=mean(Indicateur_Pic$Indicateur_Lum)
MeanArb=mean(Indicateur_Pic$Indicateur_Arb)
MeanEau=mean(Indicateur_Pic$Indicateur_Eau)

sdSpec=sd(Indicateur_Pic$Indicateur_Spec)
sdLum=sd(Indicateur_Pic$Indicateur_Lum)
sdArb=sd(Indicateur_Pic$Indicateur_Arb)
sdEau=sd(Indicateur_Pic$Indicateur_Eau)

Indicator=c("Spec","Lum","Arb","Eau")
Mean=c(MeanSpec,MeanLum,MeanArb,MeanEau)
sd=c(sdSpec,sdLum,sdArb,sdEau)
tabIndicators=data.frame(Indicator,Mean,sd)
fwrite(tabIndicators,paste0("tableIndicators",Tag,".csv"),sep=";")



