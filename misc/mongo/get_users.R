library(mongolite)
library(data.table)
library(readxl)

mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
OutF="utilisateurs_raw.csv"
#OutF="C:/Users/yvesb/Documents/www/utilisateurs_test.csv"
DirEmbargo="C:/Users/ybas/Downloads/"
PatternEmbargo="Nouvelle Charte avec Embargo"
Var=c("_id","donnees_publiques","email","google_id" 
      ,"pseudo",             "role",                  "professionnel"
      ,"nom"             ,   "prenom"        ,     "adresse"          ,  "telephone"        ,  "organisation"   ,    "charte_acceptee"   
      ,"facebook_id" ,       "commentaire"  )


FileEmbargo=list.files(DirEmbargo,PatternEmbargo,full.names=T)
FileEmbargoI=file.info(FileEmbargo)
FileEmbargo=FileEmbargo[order(FileEmbargoI$ctime)]
FileEmbargoSel=FileEmbargo[length(FileEmbargo)]
Embargo=read_xlsx(FileEmbargoSel)


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)

Sys.time()
#alldata <- users$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}')) #protocole PF
alldata <- users$find(fields='{}')
AllDataS=subset(alldata,select=Var)
Sys.time()

table(alldata$charte_acceptee)

ConfUsers=subset(alldata,alldata$donnees_publiques==F)
ConfUsers$email
OpenUsers=subset(alldata,alldata$donnees_publiques==T)
OpenUsersChelou=subset(OpenUsers,OpenUsers$charte_acceptee==F)
OpenUsersChelou$email

test40=match(AllDataS$email,Embargo$email)
summary(test40)
AllDataS$Embargo=ifelse(is.na(test40),F,T)
summary(AllDataS$Embargo)  
  
fwrite(AllDataS,OutF,sep=";")

test=subset(AllDataS,grepl("marion",AllDataS$email))
test$email

