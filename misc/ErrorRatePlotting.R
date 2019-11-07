library(data.table)
SpeciesList=fread("SpeciesList.csv")
TxErreur=fread("./Tadarida/TxErreurTadarida190410.csv")

TxErreurSp=merge(SpeciesList,TxErreur,by.x="Esp",by.y="Espece")
fwrite(TxErreurSp,paste0("TxErreurSp_",substr(Sys.time(),1,10),".csv"),sep=";")
