library(data.table)

ValidData=fread("C:/Users/yvesb/Documents/www/export_validtot230823.txt")
AlwaysValid=c("yves.bas@gmail.com") #investiguer pourquoi parfois contradiction obs/valid dans les id
SpSummaries=c("Pippip", "Pipkuh","Eptser","Nyclei", "Nycnoc", "Myodau", "Pipnat", "Barbar", "Pleaus"
              ,"Pippyg","Myonat","Myoema")

print(nrow(ValidData))
ContreValidations=subset(ValidData,ValidData$obs.espece!=""&ValidData$valid.espece!="")
ValidO=subset(ValidData,ValidData$obs.espece!="")
ValidV=subset(ValidData,ValidData$valid.espece!="")
table(ValidO$obs.proba)

#Test19=subset(ListSi,ListSi %in% c("Myocry","Myobly","Myomyo"))
ValidData$obs.espece[ValidData$obs.espece=="Myocry"]="Myonat"
ValidData$obs.espece[ValidData$obs.espece=="Myoesc"]="Myonat"
ValidData$obs.espece[ValidData$obs.espece=="Myobly"]="MyoGT"
ValidData$obs.espece[ValidData$obs.espece=="Myomyo"]="MyoGT"
ValidData$obs.espece[ValidData$obs.espece=="Myopun"]="MyoGT"
ValidData$obs.espece[ValidData$obs.espece=="Cansp."]="Lamsp."
ValidData$obs.espece[ValidData$obs.espece=="Urorug"]="Urosp"
ValidData$obs.espece[ValidData$obs.espece=="Urobre"]="Urosp"

ValidDataO=subset(ValidData,ValidData$obs.espece!="")
ListO=unique(ValidDataO$proprietaire)
ListE=unique(ValidDataO$email)

Obs=vector()
Espece=vector()
SuccessRate=vector()
Confiance=vector()
for (i in 1:length(ListE)){
  print(ListE[i])
  ValidE=subset(ValidDataO,ValidDataO$email==ListE[i])
  ValidEV=subset(ValidE,ValidE$valid.espece!="")
  ListSi=unique(ValidEV$obs.espece)
  ValidEV$success=(ValidEV$obs.espece==ValidEV$valid.espece) #idéalement il faudrait faire gaffe aux groupes d'espèces
  summary(ValidEV$success)
  if(nrow(ValidEV)>0){
    for (j in 1:length(unique(ValidEV$obs.proba))){
      ValidEVC=subset(ValidEV,ValidEV$obs.proba==unique(ValidEV$obs.proba)[j])
      EVsumm=aggregate(ValidEVC$success,by=list(ValidEVC$obs.espece),mean)
      Obs=c(Obs,rep(ListE[i],nrow(EVsumm)))
      Espece=c(Espece,EVsumm$Group.1)
      SuccessRate=c(SuccessRate,EVsumm$x)
      Confiance=c(Confiance,rep(unique(ValidEV$obs.proba)[j],nrow(EVsumm)))
      
    }
    
  }
}
table(SuccessRate)
table((SuccessRate>0.95),Espece)
table((SuccessRate>0.95),Confiance)

DataValid=data.frame(Obs,Espece,Confiance,SuccessRate)
boxplot(DataValid$SuccessRate~DataValid$Confiance)

DataValid$Valid=NA
DataValidDiag=DataValid[0,]
for (k in 1:length(ListE)){
  Datak=subset(DataValid,DataValid$Obs==ListE[k])
  if(nrow(Datak)>0){
    LSk=unique(Datak$Espece)
    for (l in 1:length(LSk)){
      Datakl=subset(Datak,Datak$Espece==LSk[l])
      Datakl=Datakl[order(Datakl$Confiance,decreasing=T),]
      Datakl$Valid[1]=(Datakl$SuccessRate[1]>0.95)
      if(nrow(Datakl)>1){
        #stop("coder plusieurs confiances")
        for (m in 2:nrow(Datakl)){
          Datakl$Valid[m]=((Datakl$SuccessRate[m]>0.95)&(Datakl$Valid[m-1]))
        }
        
      }
      DataValidDiag=rbind(DataValidDiag,Datakl)
    }  
  }  
}
DataValidDiag$Valid=ifelse(DataValidDiag$Obs %in% AlwaysValid,TRUE,DataValidDiag$Valid)
table(DataValidDiag$Valid,DataValidDiag$Confiance)
table(DataValidDiag$Valid,DataValidDiag$Espece)

fwrite(DataValidDiag,"DataValidDiag.csv",sep=";")

#summaries
#head(table(DataValidDiag$Espece)[order(table(DataValidDiag$Espece),decreasing=T)],14)
DataSumm=subset(DataValidDiag,DataValidDiag$Espece %in% SpSummaries)
boxplot(DataSumm$SuccessRate~DataSumm$Confiance)

p <- ggplot(DataSumm, aes(x=Confiance, y=SuccessRate)) + 
  geom_boxplot()
#+geom_text(aes(label =as.numeric(table(ValidO$obs.proba))), size=6)
p

DataSummSur=subset(DataSumm,DataSumm$Confiance=="SUR")
p <- ggplot(DataSummSur, aes(x=Espece, y=SuccessRate)) + 
  geom_boxplot()
p


