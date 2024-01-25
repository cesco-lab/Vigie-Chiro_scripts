library(mongolite)
library(data.table)
library(lubridate)

mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
InF="C:/Users/yvesb/Downloads/DataW.csv"

if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}

users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)

Sys.time()
#alldata <- users$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}')) #protocole PF
alldata <- users$find(fields='{}')
Sys.time()



table(alldata$charte_acceptee)

ConfUsers=subset(alldata,alldata$donnees_publiques==F)
ConfUsers$email
OpenUsers=subset(alldata,alldata$donnees_publiques==T)
OpenUsersChelou=subset(OpenUsers,OpenUsers$charte_acceptee==F)
OpenUsersChelou$email


participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#test=participations$export()
sites = mongo(collection="sites", db="vigiechiro", url=connection_string)


Sys.time()
alldatapart<-participations$find(fields='{}')
Sys.time()

Sys.time()
alldatasites<-sites$find(fields='{}')
Sys.time()

alldatasitesPF=subset(alldatasites,alldatasites$protocole=="54bd090f1d41c8103bad6252")

WaveToCheck=fread(InF)

FileInfo1=tstrsplit(WaveToCheck$files,split="-")
NumCarre=gsub("Car","",FileInfo1[[1]])

ListCarre=unique(NumCarre)

Conf=vector()
for (i in 1:length(ListCarre)){
  if(i%%100==1){print(paste(Sys.time(),i))}
  IdSite=subset(alldatasitesPF$'_id',grepl(ListCarre[i],alldatasitesPF$titre))
  #print(length(IdSite))
  if(length(IdSite)==1){
    #stop()
    Wavei=subset(WaveToCheck,NumCarre==ListCarre[i])
    FileInfoi=tstrsplit(Wavei$files,split="-")
    NumPoint=FileInfoi[[4]][1]
    PartSite=subset(alldatapart,(alldatapart$site==IdSite)&(alldatapart$point==NumPoint))
    FileInfo2=tstrsplit(Wavei$files[1],split="_")
    Date=ymd(FileInfo2[[length(FileInfo2)-2]])
    PartSite=subset(PartSite,PartSite$date_debut<=(Date+1))
    PartSite=subset(PartSite,PartSite$date_fin>=(Date-1))
    if(nrow(PartSite)>0){
    if(PartSite$observateur[1] %in% ConfUsers$'_id'){
      Conf=c(Conf,1)
    }else{
      Conf=c(Conf,0)
    }
    }else{
      Conf=c(Conf,0)
      
    }
    
  }else{
    Conf=c(Conf,0)
  }
  
}

test=match(NumCarre,ListCarre)
ConfAll=Conf[test]

WaveToCheck$confidentiel=ConfAll

fwrite(WaveToCheck,gsub(".csv","_CheckConfidential.csv",InF),sep=";")

Summary=data.frame(ListCarre,Conf)

fwrite(Summary,"SummaryConfidential.csv",sep=";")
