library(data.table)

Raw="C:/Users/yvesb/Documents/VigieChiro/Raw"
SpSel="Nycnoc"
SelRP=T
OutF="C:/Users/yvesb/Documents/VigieChiro/Exports/NycnocRP.csv"



FRaw=list.files(Raw,full.names=T)
Fexport=subset(FRaw,substr(basename(FRaw),1,7)=="export_")

datalist=list()
for (i in 1:length(Fexport))
{
  datalist[[i]]=fread(Fexport[i])
  datalist[[i]]=subset(datalist[[i]],datalist[[i]]$espece==SpSel)
  if(SelRP){
  datalist[[i]]=subset(datalist[[i]],substr(datalist[[i]]$donnee,1,3)=="Cir")
  }
  }
dataAll=rbindlist(datalist)

fwrite(dataAll,OutF,sep=";")
