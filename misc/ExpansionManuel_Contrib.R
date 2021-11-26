library(data.table)
Expansion=fread("./mnhn/ExpansionManuel.csv")
#Direct=fread("./mnhn/DirectManuel.csv")
DataAuto=fread("C:/Users/Yves Bas/Documents/GitHub/VigieChiro/data/data_vigieChiro_DataRP_SpTron_50_site_55sp_withAbs.csv")


EPP=subset(Expansion,Expansion$espece=="Pippip")

ListCir=unique(EPP$ID_CIRCUIT)

Tendance=vector()
Ndata=vector()
FYear=vector()
LYear=vector()
for (i in 1:length(ListCir))
{
  EPPi=subset(EPP,EPP$ID_CIRCUIT==ListCir[i])
  modi=lm(EPPi$Activite~EPPi$ANNEE)
  Tendance[i]=modi$coefficients[2]
  Ndata[i]=nrow(EPPi)
  FYear[i]=min(EPPi$ANNEE)
  LYear[i]=max(EPPi$ANNEE)
}
Contrib=data.table(ListCir,Tendance,Ndata,FYear,LYear)
Contrib$Contrib=-Contrib$Tendance*Contrib$Ndata
fwrite(Contrib,"ContribExpansion.csv")

