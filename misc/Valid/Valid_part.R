library(readxl)
library(data.table)
library(lubridate)

ValidSansPar=read_xlsx("./Tadarida/rounds/valid2101/SansRef/valid210111.xlsx")
#ValidSansPar=fread("./Tadarida/rounds/valid2101/Ref_participation/Valid210217all.csv")

Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")


f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

DataInfo=tstrsplit(ValidSansPar$donnee,split="-")

PF=(substr(DataInfo[[1]],1,3)=="Car")
table(PF)
nonPF=subset(ValidSansPar,!PF)
RP=(substr(DataInfo[[1]],1,3)=="Cir")
table(RP)
nonRPF=subset(ValidSansPar,!PF&!RP)
fwrite(nonRPF,paste0("ValidNonIntegrable",Sys.Date(),".csv"),sep=";")

ValidPF=subset(ValidSansPar,PF)
DataInfo=tstrsplit(ValidPF$donnee,split="-")

ValidPF$participation="missing"
#OrphanValid=ValidPF[0,]
#PartV=vector()
for (i in 1:nrow(ValidPF))
{
  Sitei=gsub("Car","Vigiechiro - Point Fixe-",DataInfo[[1]][i])
  PartS=subset(Particip,Particip$site==Sitei)
  Pointi=DataInfo[[4]][i]
  PartP=subset(PartS,PartS$point==Pointi)
  Datai=ifelse(substr(ValidPF$donnee[i],nchar(ValidPF$donnee[i])-3
                      ,nchar(ValidPF$donnee[i])-3)=="."
               ,substr(ValidPF$donnee[i]
                       ,1,nchar(ValidPF$donnee[i])-4)
               ,ValidPF$donnee[i])
  Datei=f2pPF(Datai)
  Dated=dmy_hm(PartP$date_debut)
  Datef=dmy_hm(PartP$date_fin)
  PartD=subset(PartP,(Dated-7200<Datei)&(Datef+7200>Datei))
  PartD=PartD[order(PartD$nb_don,decreasing = T),]
    ValidPF$participation[i]=PartD$participation[1]
  
}
OrphanValid=subset(ValidPF,ValidPF$participation=="missing")
ValidPar=subset(ValidPF,!ValidPF$participation=="missing")


fwrite(OrphanValid,paste0("OrphanValidwoPar",Sys.Date(),".csv"),sep=";")
fwrite(ValidPar,paste0("ValidPar",Sys.Date(),".csv"),sep=";")
