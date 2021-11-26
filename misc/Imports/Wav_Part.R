library(readxl)
library(data.table)

#Codes=read_excel("E:/Code_Vigiechiro.xlsx")
Particip=fread("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/p_export.csv")
WD="E:/PI"
WSD=dir(WD,full.names=T)


SITE=vector()
POINT=vector()
DD=vector()
DF=vector()
for (j in 92:length(WSD))
{
  WF=list.files(WSD[j],full.names=T,pattern=".wav$")
  SiteTemp=substr(basename(WF[1]),4,9)
  SITE=c(SITE,SiteTemp)
  CarreTemp=paste0("Vigiechiro - Point Fixe-",SiteTemp)
  PointTemp=substr(basename(WF[1]),22,23)
  POINT=c(POINT,PointTemp)
  DateDebut=paste0( substr(basename(WF[1])
                           ,nchar(basename(WF[1]))-16
                           ,nchar(basename(WF[1]))-15
  ),"/"
  ,substr(basename(WF[1])
          ,nchar(basename(WF[1]))-18
          ,nchar(basename(WF[1]))-17
  ),"/"
  ,substr(basename(WF[1])
          ,nchar(basename(WF[1]))-22
          ,nchar(basename(WF[1]))-19
  )," "
  ,substr(basename(WF[1])
          ,nchar(basename(WF[1]))-13
          ,nchar(basename(WF[1]))-12
  ),":"
  ,substr(basename(WF[1])
          ,nchar(basename(WF[1]))-11
          ,nchar(basename(WF[1]))-10
  )
  )
  
  DateFin=paste0( substr(basename(WF)[length(WF)]
                         ,nchar(basename(WF)[length(WF)])-16
                         ,nchar(basename(WF)[length(WF)])-15
  ),"/"
  ,substr(basename(WF)[length(WF)]
          ,nchar(basename(WF)[length(WF)])-18
          ,nchar(basename(WF)[length(WF)])-17
  ),"/"
  ,substr(basename(WF)[length(WF)]
          ,nchar(basename(WF)[length(WF)])-22
          ,nchar(basename(WF)[length(WF)])-19
  )," "
  ,substr(basename(WF)[length(WF)]
          ,nchar(basename(WF)[length(WF)])-13
          ,nchar(basename(WF)[length(WF)])-12
  ),":"
  ,substr(basename(WF)[length(WF)]
          ,nchar(basename(WF)[length(WF)])-11
          ,nchar(basename(WF)[length(WF)])-10
  )
  )
  DD=c(DD,DateDebut)
  DF=c(DF,DateFin)
  
  Pj=subset(Particip,Particip$site==CarreTemp)
  Pj=subset(Pj,Pj$point==PointTemp)
  Pj=subset(Pj,substr(Pj$date_debut,1,10)==
              substr(DateDebut,1,10))
  if(nrow(Pj)==1)
  {
    NewDir=paste0("E:/PI/",Pj$participation)
    dir.create(NewDir)
    NewName=paste0(NewDir,"/",basename(WF))
    file.rename(from=WF,to=NewName)
  }else{
    paste(WSD[j])
    stop("probleme doublon")
  }
}

pimport=data.frame(SITE,POINT,DD,DF)
fwrite(pimport,"pimport_GCP.csv",sep=";")
getwd()
