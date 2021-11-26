library(data.table)

FiList=fread("FiltOD_ParSelShifts2021-04-19.csv")
RawDir="./VigieChiro/Raw"

PrefL=substr(FiList$Part,1,3)

DataL=data.frame()
for (i in 1:length(unique(PrefL)))
{
  Rawi=fread(paste0(RawDir,"/export_",unique(PrefL)[i],".csv"))
  Fili=subset(FiList,PrefL==unique(PrefL)[i])
  Datai=subset(Rawi,Rawi$donnee %in% gsub(".wav","",Fili$FilToD))
  DataL=rbind(DataL,Datai)
}

fwrite(DataL,paste0("DataL",Sys.Date(),".csv"),sep=";")
