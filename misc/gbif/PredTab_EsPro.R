library(data.table)

EsPro=fread("./natura/especes_protegees.csv",sep=";")
PredTab=fread("PredTab_140_Presence_07_GI_SysGrid_Radius_ 90000_50000.csv")
PredTab=fread("PredTabNordMontp140.csv")


LEP=subset(EsPro$espece,EsPro$espece %in% names(PredTab))

PredEP=subset(PredTab,select=c("Group.1","Group.2",LEP))

fwrite(PredEP,"PredEP_NM.csv",sep=";")
