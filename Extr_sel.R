library(data.table)

#ETAPE 0 - IMPORT DES TABLES
#bien renommer les chemins en fonction de l'ordi utilisé
#et vérifier les versions (date, import complet ou non)
args="C:/wamp64/www/Sel_validobs_2017.csv"
  
#table "données"
DataTot=fread("C:/wamp64/www/export180227.txt")
Sys.time()

Sel=fread(args)

#tol=0 #nombre de données à garder avant/après

#test=match(Sel$Filename,DataTot$donnee)

DataSel=subset(DataTot,DataTot$donnee %in% Sel$Filename)
fwrite(DataSel,paste0(dirname(args),"/Data_",basename(args)))
