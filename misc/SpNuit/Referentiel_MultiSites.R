library(data.table)

Data=fread("./VigieChiro/Exports/SpNuit2_0_DataLP_PF_exportTot_Porquerolles_SL.csv")
Thres=0.5
Referentiel=fread("./VigieChiro/Referentiels/refPF_Eau_2020-04-10.csv")
Repr=F
Bat=T
SpeciesList=fread("SpeciesList.csv")
Tag="PorquerollesBassinsEau"
ClassAct=c("FAIBLE","MODEREE","FORTE","TRES FORTE")
FP=T
FiltrePoint=c("Z6","F2")
FC=T
FiltreCarre=c("Vigiechiro - Point Fixe-831526")

DataFiable=subset(Data,Data$score_max>0.5)
if(Repr){
  DataFiable=subset(DataFiable,substr(DataFiable$nom,1,1)!="Z")
  
  #DataRepr=subset(DataFiable,substr(DataFiable$nom,1,1)!="Z")
  #nNuitRepr=nrow(unique(cbind(DataRepr$participation,DataRepr$Nuit)))
}  

if(Bat)
{
  SpeciesInfo=match(DataFiable$espece,SpeciesList$Esp)
  IsBat=(SpeciesList$Group[SpeciesInfo]=="bat")
  #table(IsBat,DataFiable$espece)
  DataFiable=subset(DataFiable,IsBat)
}

if(FP)
{
  DataFiable=subset(DataFiable,DataFiable$nom %in% FiltrePoint)
table(DataFiable$site)
  }
if(FC)
{
  DataFiable=subset(DataFiable,DataFiable$site %in% FiltreCarre)
  table(DataFiable$site)
}


nNuit=nrow(unique(cbind(DataFiable$participation,DataFiable$Nuit)))

ScoreMax=vector()
Faible=vector()
Moderee=vector()
Forte=vector()
TresForte=vector()
for (i in 1:length(unique(DataFiable$espece)))
{
    Datai=subset(DataFiable,DataFiable$espece==unique(DataFiable$espece)[i])
    ScoreMax=c(ScoreMax,max(Datai$score_max))  
    Refi=subset(Referentiel,Referentiel$Espece==unique(DataFiable$espece)[i])  
    Faible=c(Faible,nrow(subset(Datai,Datai$nb_contacts<Refi$Q25)))
    Moderee=c(Moderee,nrow(subset(Datai,(Datai$nb_contacts>=Refi$Q25)&
                                    (Datai$nb_contacts<Refi$Q75))))
    Forte=c(Forte,nrow(subset(Datai,(Datai$nb_contacts>=Refi$Q75)&
                                    (Datai$nb_contacts<Refi$Q98))))
    TresForte=c(TresForte,nrow(subset(Datai,Datai$nb_contacts>=Refi$Q98)))
    
}
SpInfo=match(unique(DataFiable$espece),SpeciesList$Esp)
Espece=SpeciesList$NomFR[SpInfo]

DataMS=data.frame(Espece
                  ,ScoreMax,Faible,Moderee,Forte,TresForte)

fwrite(DataMS,paste0("./VigieChiro/Exports/DataMS_",Tag,".csv"),sep=";")

DataMS2=DataMS

DataMS2$NbNuits=DataMS2$Faible+DataMS2$Moderee+DataMS2$Forte+
  DataMS2$TresForte
DataMS2$Faible=round(DataMS2$Faible/DataMS2$NbNuits*100)
DataMS2$Moderee=round(DataMS2$Moderee/DataMS2$NbNuits*100)
DataMS2$Forte=round(DataMS2$Forte/DataMS2$NbNuits*100)
DataMS2$TresForte=round(DataMS2$TresForte/DataMS2$NbNuits*100)
DataMS2$'Forte+TresForte'=DataMS2$Forte+DataMS2$TresForte
DataMS2$'Moderee+Forte+TresForte'=DataMS2$Moderee+DataMS2$Forte+DataMS2$TresForte
ScoreTot=(DataMS2$TresForte>2)+(DataMS2$'Forte+TresForte'>25)+
  (DataMS2$'Moderee+Forte+TresForte'>75)
DataMS2$NbPoints=ScoreTot

DataMS2$Activite=ClassAct[ScoreTot+1]
  DataMS2$ScoreMax=NULL

  fwrite(DataMS2,paste0("./VigieChiro/Exports/DataMS2_",Tag,".csv"),sep=";")
  

