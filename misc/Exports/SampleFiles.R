library(data.table)

WavA=fread("C:/wamp64/www/wavarchivees.txt")
NbSample=300
exportDir="./VigieChiro/Raw"

WavS=WavA$V1[sample.int(nrow(WavA),NbSample)]

Slots=substr(WavS,1,3)
USlots=unique(Slots)
USlots=USlots[order(USlots)]

Part=vector()
Files=vector()
for (i in 1:length(USlots))
{
  exporti=fread(paste0(exportDir,"/export_",USlots[i],".csv"))
  WavSi=subset(WavS,Slots==USlots[i])
    for (j in 1:length(WavSi))
    {
      exportj=subset(exporti,exporti$participation==WavSi[j])
  if(nrow(exportj)>0)
  {
      Part=c(Part,WavSi[j])
      
      Filesj=unique(exportj$donnee)
    FileSel=Filesj[sample(length(Filesj),1)]  
    Files=c(Files,FileSel)
  }
    }
}
DataToImport=data.frame(Part,Files)
DataToImport$Req=paste0(DataToImport$Part,";",DataToImport$Files)
fwrite(DataToImport,"DataToImport.csv",sep=";")

