library(data.table)
DataTot=fread("C:/wamp64/www/export.txt")
LWdf=fread("LWdf.csv")
DataWAV=paste0(DataTot$donnee,".wav")


Wp=match(LWdf$NF,DataWAV)
Wpart=DataTot$participation[Wp]

ListDir=levels(as.factor(LWdf$ND))
ListPar=vector()
for (i in 1:length(ListDir))
{
  Partemp=subset(Wpart,LWdf$ND==ListDir[i])
  ParSel=unique(Partemp)
  ParSel2=subset(ParSel,!is.na(ParSel))
  if(length(ParSel2)==0)
  {
    ListPar=c(ListPar,basename(ListDir[i]))
  }else{
    if(length(ParSel2)==1)
    {
      ListPar=c(ListPar,ParSel2)
    }else{
      ListPar=c(ListPar,paste0(ParSel2[1],"_pb"))
    }
  }
}
NewDir=data.frame(cbind(ListDir,ListPar))
fwrite(NewDir,"NewDir.csv")

