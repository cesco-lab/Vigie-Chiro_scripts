library(data.table)
ListTCZ=list.files("C:/wamp64/www/tc_sel2",full.names=T)
#Particip=fread("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/p_export.csv")
#SpeciesList=fread("SpeciesList.csv")
IdSel=fread("IdSel.csv")

DataToCopy=vector()
for (i in 1:length(ListTCZ))
{
  
  FI=file.info(ListTCZ[i])
  
  if(FI$size>100)
  {
    TCdir=substr(basename(ListTCZ[i]),1,nchar(basename(ListTCZ[i]))-7)
    TClist=list.files(paste0("./",TCdir))
    Datalist=substr(TClist,1,nchar(TClist)-3)
    testsel=match(IdSel$donnee,Datalist)
    testsel2=subset(testsel,!is.na(testsel))
    Datatemp=c(Datalist[(testsel2-1)],Datalist[testsel2],Datalist[testsel2+1])
    DataToCopy=c(DataToCopy,Datatemp)
    print(paste(i,length(testsel2)))
  }
}
fwrite(as.data.frame(DataToCopy),"C:/wamp64/www/DataToCopy.csv")
