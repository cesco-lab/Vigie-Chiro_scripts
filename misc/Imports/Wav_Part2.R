library(mongolite)
library(data.table)
library(uuid)
library(jsonlite)
library(lubridate)


mongo=fread("C:/Users/Yves Bas/Desktop/mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
Dir="D:/PI_ONF_Aura"
DirOut="D:/PIr_ONF_Aura"
WavOnly=T

dir.create(DirOut)


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


#queuer_jobs = mongo(collection="queuer_jobs", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
sites = mongo(collection="sites", db="vigiechiro", url=connection_string)


Sys.time()
alldatapart<-participations$find(fields='{}')
Sys.time()
alldatasites<-sites$find(fields='{}')
Sys.time()
#alldataobs<-users$find(fields='{}')
Sys.time()


Files=list.files(Dir,recursive=T,full.names = T, pattern=".wav$")

Dirs=unique(dirname(Files))

for (i in 1:length(Dirs)){
  CorrecZ0=F
  print(i)
  print(Dirs[i])
  if(WavOnly){
    Filesi=subset(Files,dirname(Files)==Dirs[i],pattern=".wav$")
  }else{
    Filesi1=subset(Files,dirname(Files)==Dirs[i],pattern=".wav$")
    Filesi2=subset(Files,dirname(Files)==Dirs[i],pattern=".ta$")
    Filesi=ifelse(length(Filesi1)>0,Filesi1,Filesi2)
  }
  if(length(Filesi)==0){stop("pb fichiers")}
  NumCi=substr(basename(Filesi[1]),4,9)  
  print(NumCi)
  NumCi2=substr(basename(Filesi[length(Filesi)]),4,9)  
  if(NumCi!=NumCi2){stop("carre mal range")}
  sitesi=subset(alldatasites
                ,alldatasites$titre==paste0("Vigiechiro - Point Fixe-"
                                            ,NumCi))
  if(nrow(sitesi)!=1){stop("pb site")}
  parti=subset(alldatapart,alldatapart$site==sitesi$'_id')
  Infoi=tstrsplit(basename(Filesi[1]),split="_")
  Datei=ymd(substr(Infoi[[length(Infoi)-2]]
                   ,nchar(Infoi[[length(Infoi)-2]])-7
                   ,nchar(Infoi[[length(Infoi)-2]])))
  print(Datei)
  # if(is.na (Datei)){
  #   Infoi2=tstrsplit(Infoi[[length(Infoi)-2]],split="-")  
  #   Datei=ymd(  
  # }
  partisel=subset(parti,as.Date(parti$date_debut)==Datei)
  if(nrow(partisel)==0){
    if(length(Filesi)>39){
      Infoi=tstrsplit(basename(Filesi[40]),split="_")
      Datei=ymd(substr(Infoi[[length(Infoi)-2]]
                       ,nchar(Infoi[[length(Infoi)-2]])-7
                       ,nchar(Infoi[[length(Infoi)-2]])))
      print(Datei)
      partisel=subset(parti,as.Date(parti$date_debut)==Datei)
    }
  }
  
  Pointi=tstrsplit(basename(Filesi[1]),split="-")[[4]]
  print(Pointi)
  Pointi2=tstrsplit(basename(Filesi[length(Filesi)]),split="-")[[4]]
  if(Pointi!=Pointi2){
    Pointic=gsub("Z0","Z",Pointi)
    Pointic2=gsub("Z0","Z",Pointi2)
    if(Pointic==Pointic2){
      CorrecZ0=T
      Pointi=Pointic
    }else{
      stop("point mal range")
    }
    
  }
  partisel2=subset(partisel,partisel$point==Pointi)
  
  if(nrow(partisel2)>1){
    #stop("pb participation")
    partisel2=subset(partisel,!is.na(partisel$traitement$etat))
  }
  
  if(nrow(partisel2)!=1){
    if(nrow(partisel2)==0){stop("pb participation")}
    Infoi=tstrsplit(basename(Filesi[length(Filesi)]),split="_")
    Datei=ymd(Infoi[[length(Infoi)-2]])
    print(Datei)
    partisel2=subset(partisel2,as.Date(partisel2$date_fin)==Datei)
    if(nrow(partisel2)==0){stop("pb date fin")}
    if(nrow(partisel2)>1){
      
      p=1
      testp=0
      while((testp==0)|(p<=nrow(partisel2))){
        testp=grepl(partisel2$'_id'[p],basename(Dirs[i]))
        p=p+1
      }
      if(!testp){stop("pb date fin 2")}
      partisel2=partisel2[p-1,]
    }
    
  }
  
  dir.create(paste0(DirOut,"/",sitesi$'_id'))
  dir.create(paste0(DirOut,"/",sitesi$'_id',"/",partisel2$'_id'))  
  NewFiles=paste0(DirOut,"/",sitesi$'_id',"/",partisel2$'_id',"/"
                  ,basename(Filesi))
  if(CorrecZ0){NewFiles=paste0(dirname(NewFiles),"/"
                               ,gsub("-Z0","-Z",basename(NewFiles)))}
  test=file.rename(from=Filesi,to=NewFiles)
  if(mean(test)<1){stop("copie")}  
}


