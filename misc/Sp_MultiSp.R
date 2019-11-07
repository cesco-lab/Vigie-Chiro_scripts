library(data.table)
SpeciesList=fread("SpeciesList.csv")
Ref=fread("refPF_Seuil90.csv")
FDataAct="SpNuit2_Seuil90_DataLP_PF_exportTot.csv"
DataAct=fread(FDataAct)

Seuilocc=100
Group=(c("bush-cricket"))

SelGroup=(SpeciesList$Group %in% Group)
SelList=subset(SpeciesList,SelGroup)
SelOcc=subset(Ref,Ref$nbocc>Seuilocc)
SelList=subset(SelList,SelList$Esp %in% SelOcc$Espece)

#TRES MOCHE mais efficace...
DataNuit=unique(as.data.table(DataAct),by=c("participation","Nuit","num_micro"))
NuitEch=paste(DataNuit$participation,DataNuit$Nuit,DataNuit$num_micro)


DataMS=DataAct[0,]
for(i in 1:nrow(SelList))
{
  print(Sys.time())
  print(levels(as.factor(SelList$Esp))[i])
  SpData=subset(DataAct
                ,DataAct$espece==levels(as.factor(SelList$Esp))[i])
  NuitPart=paste(SpData$participation,SpData$Nuit,SpData$num_micro)
  Sp0=subset(DataNuit,!NuitEch %in% NuitPart)
  Sp0$nb_contacts=0
  SpData_w0=rbind(SpData,Sp0)
  SpData_w0$espece=levels(as.factor(SelList$Esp))[i]
  DataMS=rbind(DataMS,SpData_w0)
}

ExpName=paste0("./VigieChiro/ModPred/Data/",substr(FDataAct,1,15),"_",Group,"_",Seuilocc,".csv")
fwrite(DataMS,ExpName,sep=";")
