library(mongolite)
library(data.table)
library(lubridate)
library(StreamMetabolism)

mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
InF="DataW.csv"

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
WaveToCheck=subset(WaveToCheck,grepl("\\.wav",WaveToCheck$files))

FileInfo1=tstrsplit(WaveToCheck$files,split="-")
NumCarre=gsub("Car","",FileInfo1[[1]])
ListCarre=unique(NumCarre)
Pref=substr(WaveToCheck$files,1,24)

ListPref=unique(Pref)

Nocturnal=vector()
for (i in 1:length(ListPref)){
  if(i%%100==1){print(paste(Sys.time(),i))}
  #print(i)
  Wavei=subset(WaveToCheck,Pref==ListPref[i])
  FileInfo1=tstrsplit(Wavei$files,split="-")
  NumCarre=gsub("Car","",FileInfo1[[1]])
  IdSite=subset(alldatasitesPF$'_id',grepl(NumCarre[1],alldatasitesPF$titre))
  #print(length(IdSite))
  if(length(IdSite)==1){
    #stop()
    FileInfoi=tstrsplit(Wavei$files,split="-")
    NumPoint=FileInfoi[[4]][1]
    PartSite=subset(alldatapart,(alldatapart$site==IdSite)&(alldatapart$point==NumPoint))
    FileInfo2=tstrsplit(Wavei$files[1],split="_")
    Date=ymd(FileInfo2[[length(FileInfo2)-2]])
    if(!is.na(Date)){
      Hour1=ymd_hms(paste(FileInfo2[[length(FileInfo2)-2]],FileInfo2[[length(FileInfo2)-1]]))
      FileInfoLast=tstrsplit(Wavei$files[nrow(Wavei)],split="_")
      Hour2=ymd_hms(paste(FileInfoLast[[length(FileInfoLast)-2]],FileInfoLast[[length(FileInfoLast)-1]]))
      Date2=ymd(FileInfoLast[[length(FileInfoLast)-2]])
      #print(Hour1)
      #print(Hour2)
      Sitei=subset(alldatasitesPF,alldatasitesPF$'_id'==IdSite)
      Pointi=subset(Sitei$localites[[1]],Sitei$localites[[1]]$nom==NumPoint)
      
      if((nrow(Pointi)>0)&(!is.na(Hour1))&(!is.na(Hour2))){
        if((!is.null(Pointi[[1]]))){
          Srs=sunrise.set(Pointi$geometries$geometries[[1]]$coordinates[[1]][1]
                          ,Pointi$geometries$geometries[[1]]$coordinates[[1]][2]
                          ,Date,timezone="CET")
          
          Dusk=((Hour1<Srs$sunset+7200)&(hour(Hour1)>12))
          Dawn=((Hour2>Srs$sunrise-7200)&(hour(Hour2)<12))
          
          if(Dawn){
            Nocturnal=c(Nocturnal,"Dawn")
            
          }else{
            if(Dusk){
              Nocturnal=c(Nocturnal,"Dusk")
              
            }else{
              if(Date==Date2){
                Nocturnal=c(Nocturnal,"Night")
              }else{
                Nocturnal=c(Nocturnal,"unknown1")
                
              }
              
            }
          }
        }else{
          Nocturnal=c(Nocturnal,"unknown2")
          
        }
        
      }else{
        Nocturnal=c(Nocturnal,"unknown3")
        
      }
    }else{
      Nocturnal=c(Nocturnal,"unknown4")
      
    }
  }else{
    Nocturnal=c(Nocturnal,"unknown5")
    
  }
  print(Nocturnal[length(Nocturnal)])
}

table(Nocturnal)
test=match(Pref,ListPref)
WaveToCheck$period=Nocturnal[test]

fwrite(WaveToCheck,gsub(".csv","_Period.csv",InF),sep=";")

Summary=data.frame(ListPref,Nocturnal)

fwrite(Summary,"SummaryPeriod.csv",sep=";")
