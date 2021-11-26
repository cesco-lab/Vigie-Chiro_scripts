library(data.table)

FSel="DCValid.csv"
WavDir="C:/wamp64/www/wav_valid"
RawData="D:/VigieChiro/Raw"
NPart="participation"
EVT=fread("C:/wamp64/www/export_validtot201130.txt")
Nwav="donnee"
ext=F

SelData=fread(FSel)
test=subset(SelData,substr(SelData$FilToD,1,6)=="Cir17-")

WavF=list.files(WavDir)
WavD=gsub(".wav","",WavF)
WData=subset(SelData,select=Nwav)
names(WData)="wav"
if(!ext)
{
  WData$wav=paste0(WData$wav,".wav")
}
SelData=subset(SelData,WData$wav %in% basename(WavF))


PData=subset(SelData,select=NPart)
names(PData)="participation"
ListPref=unique(substr(PData$participation,1,3))
ListPref=ListPref[order(ListPref)]

DataSel=data.frame()
for (i in 1:length(ListPref))
{
  Prefd=fread(paste0(RawData,"/export_",ListPref[i],".csv"))
  print(paste(Sys.time(),ListPref[i]))
  PrefSel=subset(Prefd,Prefd$donnee %in% WavD)
DataSel=rbind(DataSel,PrefSel)
}
names(DataSel)[10]="temps_fin"
DataSel=subset(DataSel,select=c(names(DataSel)[2],names(DataSel)[1]
                              ,names(DataSel)[3:ncol(DataSel)]))
test=subset(DataSel,substr(DataSel$donnee,1,6)=="Cir17-")
DataSel=DataSel[order(DataSel$donnee)]
testevt=match(paste(DataSel$donnee,DataSel$espece)
              ,paste(EVT$donnee,EVT$espece))


DataSel$obs.espece=EVT$obs.espece[testevt]
DataSel$obs.proba=EVT$obs.proba[testevt]
DataSel[is.na(DataSel)]=""

if("FilToD" %in% names(SelData)){
 SelData$donnee=gsub(".wav","",SelData$FilToD)
 SelData$FilToD=NULL
 SelData$Part=NULL
 SelData$ToD=NULL
 
 DataSel=merge(DataSel,SelData,by="donnee",all.x=T)
 }
fwrite(DataSel,paste0("DS_",FSel),sep=";")

