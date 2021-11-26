library(readxl)

#DirD="./chiros/"
#ListD=list.files(DirD,pattern="Saisie")
DataOld=read_xlsx("./chiros/donnees_capture_YB180417.xlsx")
StationsOld=read_xlsx("./chiros/donnees_capture_YB180417.xlsx",sheet = 2)
ListDnew=c("./chiros/Saisie_donnees_capture_2018_yb.xlsx"
        ,"./chiros/Saisie_donnees_capture_2019_yb.xlsx")

Data02=merge(DataOld,StationsOld,by.x=c("Date","N° filet")
             ,by.y=c("Date","Numéro_filet"))

DataN2=data.frame()
for (i in 1:length(ListD))
{
  DataNi=read_xlsx(ListDnew[i])
  StationsNi=read_xlsx(ListDnew[i],sheet = 2,skip=1)
  DataNi2=merge(DataNi,StationsNi,by.x="N° station",by.y="...1")
  DataN2=rbind(DataN2,DataNi2)
}

DataAll=rbindlist(list(Data02,DataN2),use.names=T,fill=T)
DataAll$espece=paste0(toupper(substr(DataAll$Taxon, 1, 1))
                      , tolower(substr(DataAll$Taxon, 2
                                       , nchar(DataAll$Taxon))))

fwrite(DataAll,"./chiros/DataAllCapture.csv",sep=";")
