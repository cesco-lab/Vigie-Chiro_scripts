library(data.table)
library(glmmTMB)

DataRPPF=fread("C:/Users/ybas/Downloads/CalculsTendance/data/processed/Sites Repetes/data03_SR_TP_idManual_2024-03-13_151440.csv")
DetailRP=fread("DataLP_RP.csv")
SpeciesList=fread("C:/Users/ybas/Documents/Tadarida/Tadarida-C/tadaridaC_src/other_inputs/SpeciesList.csv")


DataRP=subset(DataRPPF,DataRPPF$protocole!="POINT_FIXE")
DataPF=subset(DataRPPF,DataRPPF$protocole=="POINT_FIXE")

DataRP$micro_droit=(DataRP$num_micro=="1")

BatList=subset(SpeciesList$Esp,SpeciesList$Group=="bat")

DetailRP$DurSeq=DetailRP$temps_fin-DetailRP$temps_debut



#filter out unappropriate sample rates
# DataRP=subset(DataRP,DataRP$SampleRate<=DataRP$seuilSR_sup)
# DataRP=subset(DataRP,DataRP$SampleRate>=DataRP$seuilSR_inf)
#filter out unreliable recordings
#test=unique(cbind(DataRP$PropPip_good,DataRP$participation,DataRP$expansion_direct))
#test2=unique(cbind(DataRP$participation,DataRP$expansion_direct))
#DataRP=subset(DataRP,(DataRP$PropPip_good&DataRP$DurPip_good))
DataRPd=subset(DataRP,DataRP$expansion_direct=="direct")
ListSp=unique(DetailRP$espece)
ListSp=subset(ListSp,ListSp %in% BatList)

DetailRP$num_micro=tstrsplit(DetailRP$donnee,split="_")[[2]]
DetailRP$micro_droit=(DetailRP$num_micro=="1")

table(DataRPd$micro_droit)

DetailRP$ParMic=paste(DetailRP$participation,DetailRP$micro_droit)
table(DetailRP$micro_droit)
DataRPd$ParMic=paste(DataRPd$participation,DataRPd$micro_droit)
DetailDirect=subset(DetailRP,DetailRP$ParMic %in% DataRPd$ParMic)

DSM=aggregate(DetailDirect$DurSeq,by=list(DetailDirect$participation),FUN=max)
hist(DSM$x)
DSMt=aggregate(DetailRP$DurSeq
               ,by=list(DetailRP$participation,DetailRP$micro_droit),FUN=max)
hist(DSMt$x)
DSMt$directDS=DSMt$x>0.5
DetailPip=subset(DetailRP,substr(DetailRP$espece,1,3)=="Pip")
ProPip=aggregate(DetailPip$probabilite
                 ,by=list(DetailPip$participation,DetailPip$micro_droit)
                 ,FUN=function(x) quantile(x,0.95))
hist(ProPip$x,breaks=100)
ProPip$ProbCorrect=(ProPip$x>0.85)

DirectRomain=aggregate(DataRP$expansion_direct
                       ,by=list(DataRP$participation
                                ,DataRP$micro_droit)
                       ,FUN=function(x) mean(x=="direct"))
hist(DirectRomain$x)
DirectRomain$DirectRomain=DirectRomain$x
testDirect=merge(DirectRomain,DSMt,by=c("Group.1","Group.2"),all=T)
table(testDirect$DirectRomain,testDirect$directDS)
testDirect$DirectRomain[is.na(testDirect$DirectRomain)]="NA"
testDirect$directDS[is.na(testDirect$directDS)]="NA"



YearEffects=data.frame()
for (i in 1:length(ListSp))
{
  Detaili=subset(DetailDirect,DetailDirect$espece==ListSp[i])
  #Detaili$DurSeq=Detaili$temps_fin-Detaili$temps_debut
  Detaili$year=as.numeric(substr(Detaili$date_debut,1,4))
  Detaili50=subset(Detaili,Detaili$probabilite>0.5)
  if(nrow(Detaili50)>0)
  {
  #  m1=glmmTMB(DurSeq~year+(1|participation/site)+(1|detecteur_enregistreur_type)
    m1=glmmTMB(DurSeq~year+(1|LocaPartData)+(1|detecteur_enregistreur_type)
                              ,data=Detaili)
    YearEffecti=summary(m1)$coefficients$cond[2,]
    YearEffecti$species=ListSp[i]
    YearEffecti$AvDur=mean(Detaili50$DurSeq)
    YearEffecti$SdDur=sd(Detaili50$DurSeq)
    YearEffects=rbindlist(list(YearEffects,YearEffecti))
    print(paste(Sys.time(),i))
  }
}
barCenters=barplot(YearEffects$Estimate,names.arg=YearEffects$species,las=2)
arrows(barCenters, YearEffects$Estimate-1.96*YearEffects$'Std. Error',
       barCenters, YearEffects$Estimate+1.96*YearEffects$'Std. Error'
       ,angle=90,code=3)

#estimates of detectability / participation
DetailBat=subset(DetailDirect,DetailDirect$espece %in% BatList)
#DetailBat$DurSeq=DetailBat$temps_fin-DetailBat$temps_debut
hist(DetailBat$DurSeq,breaks=50)
DetailBat$year=as.numeric(substr(DetailBat$date_debut,1,4))
DetailBat$yearScaled=scale(DetailBat$year)
#DetailBat$LocaPartData=factor(DetailBat$LocaPartData)
# length(unique(DetailBat$LocaPartData))
# str(DetailBat)
mbat=glmmTMB(DurSeq~yearScaled+(1|LocaPartData)+
               (1|detecteur_enregistreur_type)+(1|espece)
      ,data=DetailBat)
VarAn=summary(mbat)$coefficients$cond[2,1]/
  summary(mbat)$coefficients$cond[1,1]*100/sd(DetailBat$year)
IntAn=summary(mbat)$coefficients$cond[2,2]*
  1.96/summary(mbat)$coefficients$cond[1,1]*100/sd(DetailBat$year)
print(paste0("Variation estimée de la durée de séquence : ",round(VarAn,2)
      ," +/- ",round(IntAn,2)," %/an"))
print(paste0("Variation estimée de la durée de séquence : "
             ,round(VarAn*(max(DetailBat$year)-min(DetailBat$year)),2)
             ," +/- ",round(IntAn*(max(DetailBat$year)-min(DetailBat$year)),2)
             ," % sur toute la période"))
Log=c(paste0("Variation estimée de la durée de séquence : ",round(VarAn,2)
             ," +/- ",round(IntAn,2)," %/an"),(paste0("Variation estimée de la durée de séquence : "
             ,round(VarAn*(max(DetailBat$year)-min(DetailBat$year)),2)
             ," +/- ",round(IntAn*(max(DetailBat$year)-min(DetailBat$year)),2)
             ," % sur toute la période"))
      ,paste0("Duree moyenne séquence : "
              ,round(summary(mbat)$coefficients$cond[1,1],1)
              ," secondes"))
fwrite(as.data.table(Log),"LogBiaisDurSeq.txt",sep=";")
boxplot(DetailBat$DurSeq~DetailBat$espece,las=2)

#mpart=glmmTMB(DurSeq~year+(1|participation/site)+(1|detecteur_enregistreur_type)
 #             ,data=Detaili)

#for Points Fixes
