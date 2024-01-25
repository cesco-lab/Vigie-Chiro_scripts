library(data.table)


DirW="F:/2021VIGICHIRO"
Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv")

ListDirP=dir(DirW,full.names=T)
ListDirP=subset(ListDirP,!grepl("~",ListDirP))

for (i in 22:length(ListDirP)){
  LWi=list.files(ListDirP[i],full.names=T,pattern=".wav$",ignore.case = T)
  if(length(LWi)==0){
    Ld=list.files(ListDirP[i],full.names=T,ignore.case = T)
    LWi=list.files(Ld[1],full.names=T,pattern=".wav$",ignore.case = T)
  }
  
  NumCarre=gsub("Car","",tstrsplit(basename(LWi[1]),split="-")[[1]])
  print(NumCarre)
  if(nchar(NumCarre)==5){NumCarre=paste0("0",NumCarre)}
  Partc=subset(Particip,grepl(NumCarre,Particip$site))
  Jour=substr(LWi[1],nchar(LWi[1])-16,nchar(LWi[1])-15)
  Mois=substr(LWi[1],nchar(LWi[1])-18,nchar(LWi[1])-17)
  Annee=substr(LWi[1],nchar(LWi[1])-22,nchar(LWi[1])-19)
  Date=paste0(Jour,"/",Mois,"/",Annee)
  print(Date)
  print(Partc$date_debut)
  Partd=subset(Partc,grepl(Date,Partc$date_debut))
  print(Partd$date_debut)
  Point=tstrsplit(basename(LWi[1]),split="-")[[4]]
  print(Point)
  Partp=subset(Partd,Partd$point==Point)
if(nrow(Partp)!=1){stop("pb correspondance participation")}
  NewNamei=gsub("Car7","Car07",LWi)
  NewNamei=gsub("Pass-1","Pass1",NewNamei)
  NewNamei=paste0(dirname(ListDirP[i]),"/",Partp$idsite[1],"/",Partp$participation[1],"/",basename(NewNamei))
  print(head(NewNamei))
  
  dir.create(dirname(dirname(NewNamei[1])))
  dir.create((dirname(NewNamei[1])))
  test=file.rename(from=LWi,to=NewNamei)
  summary(test)
  if(min(test)==0){stop("copy problem")}
}
