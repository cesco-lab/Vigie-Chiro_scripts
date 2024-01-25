library(data.table)
library(ggplot2)

ValidData=fread("C:/Users/yvesb/Documents/www/export_validtot230823.txt")
SpNuitNaive=fread("SpNuit_2DI_0_DataLP_PF_exportTot.csv")
DataValidDiag=fread("DataValidDiag.csv")


SpNuitNaive=subset(SpNuitNaive,SpNuitNaive$nb_contacts_nd>0)

ListSp=unique(SpNuitNaive$espece)

Apurger=subset(DataValidDiag,DataValidDiag$Valid==F)

match1=paste(ValidData$email,ValidData$obs.espece,ValidData$obs.proba)
match2=paste(Apurger$Obs,Apurger$Espece,Apurger$Confiance)
summary(match1 %in% match2)

ValidData_purge=subset(ValidData,!(match1 %in% match2))
ValidData_purge$VO.espece=ifelse(ValidData_purge$valid.espece==""
                                 ,ValidData_purge$obs.espece,ValidData_purge$valid.espece)
ValidData_purge$VO.proba=ifelse(ValidData_purge$valid.espece==""
                                ,ValidData_purge$obs.proba,ValidData_purge$valid.proba)

ListSp=ListSp[order(ListSp)]


Intercept=rep(NA,length(ListSp))
Slope=rep(NA,length(ListSp))
for (k in 1:length(ListSp)){
  #  for (k in 1:51){
  
  print(ListSp[k])
  Spk=subset(SpNuitNaive,SpNuitNaive$espece==ListSp[k])
  Validk=subset(ValidData_purge,ValidData_purge$espece==ListSp[k])
  Validk$SuccesTadarida=(Validk$espece==Validk$VO.espece)
  summary(Validk$SuccesTadarida)
  match25=paste(Spk$participation,Spk$espece,Spk$score_max)
  match26=paste(Validk$participation,Validk$espece,Validk$probabilite)
  test27=match(match25,match26)
  summary(test27)
  Spk$SuccesTadarida=Validk$SuccesTadarida[test27]
  SpkValide=subset(Spk,!is.na(Spk$SuccesTadarida))
  if(nrow(SpkValide)>0){
    #boxplot(SpkValide$score_max~SpkValide$SuccesTadarida)
    #boxplot(SpkValide$nb_contacts_nd~SpkValide$SuccesTadarida,log="y",ylim=c(1,10000),main=ListSp[k])
    print(ggplot(SpkValide, aes(x = nb_contacts_nd, y = score_max, color = SuccesTadarida)) +
            geom_point()+
            scale_x_continuous(trans='log10')+
            ggtitle(ListSp[k])
    )
    if((length(unique(SpkValide$SuccesTadarida))>1)
      &(length(unique(SpkValide$score_max))>1)){
      Mod=glm(SpkValide$SuccesTadarida~SpkValide$score_max)
      Intercept[k]=summary(Mod)$coefficients[1,1]
      Slope[k]=summary(Mod)$coefficients[2,1]
    }
  }
}
summary(Intercept)
summary(Slope)

DataCorrection=data.frame(Espece=ListSp,Intercept,Slope)

fwrite(DataCorrection,"DataCorrection.csv",sep=";")
