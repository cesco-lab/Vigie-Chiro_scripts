library(data.table)
library(glmmTMB)

DataAct=fread("C:/wamp64/www/SpNuit2_0_DataLP_PF_exportTot.csv")
Threshold=0.5
GI=fread("C:/wamp64/www/GI_sites_localites.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Particip=fread("C:/wamp64/www/p_export.csv")

VarHabPt=subset(names(GI),grepl("SpHO",names(GI)))
VarHabPt=subset(VarHabPt
                ,substr(VarHabPt,nchar(VarHabPt),nchar(VarHabPt))=="S")
GIHP=subset(GI,select=VarHabPt)
GI$CLASS=apply(GIHP,MARGIN=1,function(x) which.max(x))
Check=apply(GIHP,MARGIN=1,max)
summary(Check)
GI$Wood=GI$SpHO16S+GI$SpHO17S
GI$Water=((GI$SpCP2S+GI$SpCP4S+GI$SpCC6S)>0)
summary(GI$Water)

PSL=merge(Particip,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
PSLGI=merge(PSL,GI,by=c("longitude","latitude"))

DataThres=subset(DataAct,DataAct$score_max>Threshold)
ListPartNuit=unique(data.frame(cbind(DataThres$participation
                                     ,DataThres$Nuit)))

SSI=vector()
RepAlan=vector()
RepWood=vector()
RepWater=vector()
ActMoy=vector()
for (i in 1:length(unique(DataThres$espece)))
{
  Datai=subset(DataThres,DataThres$espece==unique(DataThres$espece)[i])
  Datai_w0=merge(Datai,ListPartNuit,by.x=c("participation","Nuit")
                 ,by.y=c("X1","X2"),all.y=T)
  Datai_w0$nb_contacts[is.na(Datai_w0$nb_contacts)]=0
  ActMoy=c(ActMoy,mean(Datai_w0$nb_contacts))
  DataGI=merge(Datai_w0,PSLGI,by="participation")
  ActClass=aggregate(DataGI$nb_contacts,by=list(DataGI$CLASS),mean)
  barplot(ActClass$x,main=unique(DataThres$espece)[i])
  SSIi=sd(ActClass$x)/mean(ActClass$x)
  SSI=c(SSI,SSIi)
  #response to ALAN
  mAlan=glmmTMB(DataGI$nb_contacts~DataGI$SpALAN_M,family="nbinom2")
  summary(mAlan)
  RepAlani=summary(mAlan)$coefficients$cond[2,1]
  RepAlan=c(RepAlan,RepAlani)
  
  #response to woodland
  mWood=glmmTMB(DataGI$nb_contacts~DataGI$Wood,family="nbinom2")
  summary(mWood)
  RepWoodi=summary(mWood)$coefficients$cond[2,1]
  RepWood=c(RepWood,RepWoodi)
  
  #response to water
  mWater=glmmTMB(DataGI$nb_contacts~DataGI$Water,family="nbinom2")
  summary(mWater)
  RepWateri=summary(mWater)$coefficients$cond[2,1]
  RepWater=c(RepWater,RepWateri)
  
  
  
  
}

plot(abs(RepWood),ActMoy)

TraitsSpecies=data.frame(Species=unique(DataThres$espece),SSI,RepAlan
                         ,RepWood,RepWater,ActMoy)
hist(TraitsSpecies$SSI)
hist(TraitsSpecies$RepAlan)
hist(TraitsSpecies$RepWood)
hist(TraitsSpecies$RepWater)

fwrite(TraitsSpecies,"./VigieChiro/Traits/HabitatTraitsSpecies.csv",sep=";")
