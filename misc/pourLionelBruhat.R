library(data.table)
test=fread("Demande_extraction_YB.csv")
SpTarget="Myonat"
RawDir="D:/VigieChiro/Raw"

Prefsel=substr(test$participat,1,3)
UPrefsel=unique(Prefsel)

Fichier=vector()
Part=vector()
for (i in 1:length(UPrefsel))
{
  eData=fread(paste0(RawDir,"/export_",UPrefsel[i],".csv"))
  Parti=subset(test$participat,Prefsel==UPrefsel[i])
  Parti=unique(Parti)
  for (j in 1:length(Parti))
  {
    pData=subset(eData,eData$participation==Parti[j])
    sData=subset(pData,pData$espece==SpTarget)
    mData=subset(sData,sData$probabilite==max(sData$probabilite))
    Fichier=c(Fichier,mData$donnee[1])
    Part=c(Part,mData$participation[1])
  }
}
DataToD=data.frame(cbind(Fichier,Part))
DataToD$ToD=paste(Part,Fichier,sep=";")
fwrite(DataToD,"DataToD.csv",sep=";")
