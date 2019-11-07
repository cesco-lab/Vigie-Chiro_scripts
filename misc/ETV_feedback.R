library(data.table)
ETV=fread("export_validtot2019-02-21.csv")
Year=2018

#Particip=fread("C:/wamp64/www/p_export.csv")

#P_obs=subset(Particip,Particip$observateur!="Yves Bas")

ETVY=subset(ETV,grepl(as.character(Year),ETV$donnee))

ETVY_obs=subset(ETVY,ETVY$proprietaire != "Yves Bas")
ETVY_obs2=subset(ETVY_obs,ETVY_obs$obs.espece!="")

SelTot=ETVY_obs2[0,]
for (i in 1:nlevels(as.factor(ETVY_obs2$proprietaire)))
{
  Subtemp1=subset(ETVY_obs2,ETVY_obs2$proprietaire==levels(as.factor(ETVY_obs2$proprietaire))[i])
for (j in 1:nlevels(as.factor(Subtemp1$obs.espece)))
{
  Subtemp2=subset(Subtemp1,Subtemp1$obs.espece==levels(as.factor(Subtemp1$obs.espece))[j])
  for (h in 1:nlevels(as.factor(Subtemp2$obs.proba)))
  {
    Subtemp3=subset(Subtemp2,Subtemp2$obs.proba==levels(as.factor(Subtemp2$obs.proba))[h])
    Seltemp=Subtemp3[sample(nrow(Subtemp3),1),]
  SelTot=rbind(SelTot,Seltemp)
  #print(paste(i,j,h))
  }
}
  }

fwrite(SelTot,paste0("SelTot_feedback_",Year,".csv"))
