library(data.table)

DataAct=fread("./VigieChiro/pourTendances/data_vigieChiro_DataRP_SpTron_90_site_54sp_withAbs.csv")
#DataActTron=fread("./VigieChiro/pourTendances/data_vigieChiro_DataRP_SpTron_90_TronPoint_54sp_withAbs.csv")
#Sample=subset(DataActTron,DataActTron$participation=="57ac88dbdd4411000e4b764e")
DirToCopy="./Vigiechiro/DataSp/RPCirw0_90"


ListSp=unique(DataAct$espece)

for (i in 1:length(ListSp))
{
DataSp=subset(DataAct,DataAct$espece==ListSp[i])
fwrite(DataSp,paste0(DirToCopy,"/",ListSp[i],".csv"))
}