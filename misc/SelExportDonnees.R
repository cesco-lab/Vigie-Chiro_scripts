library(data.table)
#library(StreamMetabolism)


#args="PrefPart" #criteria (PrefPart is the first two character of participation code - easy split of the large export.txt table)
#args="Valid"
#args[2]=list("55") #selection
#args[10]="E:"
#FDT="C:/wamp64/www/export_190803_190907.txt"

#ETAPE 0 - IMPORT DES TABLES
#bien renommer les chemins en fonction de l'ordi utilisé
#et vérifier les versions (date, import complet ou non)

#table "données"
Sys.time()
if (!exists("DataTot")){
DataTot=fread(FDT)
}
Sys.time()

if(args[1]=="PrefPart")
{
  if (!exists("PrefPart")){
  PrefPart=substr(DataTot$participation,1,3)
  }
  DataExp=subset(DataTot,PrefPart %in% args[2])
}

if(args[1]=="Valid")
{
  args[2]=paste0("validtot",substr(Sys.time(),1,10))
  DataExp=subset(DataTot,(DataTot$obs.espece!="")
                 |(DataTot$valid.espece!=""))
}

fwrite(DataExp,paste0(args[10],"/export_",args[2][[1]],".csv"))
