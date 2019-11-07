library(data.table)
GroupList=fread("GroupList.csv")
FDataAct="SpNuit2_50_DataLP_PF_exportTot.csv"

DataAct=fread(FDataAct)

DataList=list()
for (i in 1:nlevels(as.factor(GroupList$Groupe)))
{
  ListSpTemp=subset(GroupList
                    ,GroupList$Groupe==levels(as.factor(GroupList$Groupe))[i])

  DataGroup=subset(DataAct,DataAct$espece %in% ListSpTemp$Esp) 
  SumAct=aggregate(DataGroup$nb_contacts,by=c(list(DataGroup$participation)
                                              ,list(DataGroup$Nuit)
                                              ,list(DataGroup$num_micro))
                   ,sum)
  SumAct$Groupe=levels(as.factor(GroupList$Groupe))[i]
  DataList[[i]]=SumAct
  print(levels(as.factor(GroupList$Groupe))[i])
}
DataGroupTot=rbindlist(DataList)
colnames(DataGroupTot)=c("participation","Nuit","num_micro","nb_contacts"
                         ,"espece")

Name=gsub("Sp","Gp",FDataAct)
fwrite(DataGroupTot,Name,sep=";")
