library(data.table)
library(readxl)

#ETAPE 0 - IMPORT DES TABLES
#bien renommer les chemins en fonction de l'ordi utilisé
#et vérifier les versions (date, import complet ou non)
args="./Tadarida/rounds/compli_validation190220/Round190220.csv"

#table "données"
DataTot=fread("C:/wamp64/www/export.txt")
Sys.time()
Tag=F
LocTag="C:/wamp64/www/SelValid190221.csv"
Merge=T


if(Tag){
FTag=fread(LocTag)
FTag$Sel="x"
FTag=unique(FTag)
}


names(DataTot)[10]="temps_fin"

Sel=fread(args)
test1=match("Filename",names(Sel))
test2=match("donnee",names(Sel))
if(is.na(test1))
{
  if(is.na(test2))
  {
    names(Sel)[1]="Filename"
  }else{
    names(Sel)[test2]="Filename"
  }
}

if(grepl(".wav",Sel$Filename[1]))
{
  Sel$Filename=substr(Sel$Filename,1,nchar(Sel$Filename)-4)
}

#tol=0 #nombre de données à garder avant/après

#test=match(Sel$Filename,DataTot$donnee)

DataSel=subset(DataTot,DataTot$donnee %in% Sel$Filename)


if(Tag)
{
DataSel2=merge(DataSel,FTag,by="donnee",all.x=T)
}else{
  DataSel2=DataSel
}


if(Merge)
{
  DataSel2=merge(DataSel2,Sel,by.x=c("donnee","espece","frequence")
                 ,by.y=c("Filename","SpMaxF2","FreqM"),all.x=T)
}

fwrite(DataSel2,paste0(dirname(args),"/Data_",basename(args)))
