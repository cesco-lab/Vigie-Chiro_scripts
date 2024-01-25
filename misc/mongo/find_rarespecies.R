library(mongolite)
library(data.table)
library(beepr)
library(uuid)
library(jsonlite)
library(rjson)


connection_string = "mongodb://vigiechiro:4Zp9nh8d2YSVt5@ccmg01.in2p3.fr:27030,ccmg02.in2p3.fr:27030,ccmg03.in2p3.fr:27030/vigiechiro?replicaSet=rs0&readPreference=secondaryPreferred&socketTimeoutMS=6000000"
#connection_string = "mongodb://vigiechiro:kafc2xdr8vZdBpuV@ccdbmgtstno01.in2p3.fr:27080,ccdbmgtstno03.in2p3.fr:27080,ccdbmgtstno02.in2p3.fr:27080/vigiechiro?replicaSet=rs0&authSource=vigiechiro&socketTimeoutMS=6000000"
#id_observateur="5e9886c590250e001113d95d" #VC mnhn
#id_observateur="558acd059dcbdc000e0793ee" #Yves Bas


#sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
donnee_obs = mongo(collection="donnees", db="vigiechiro", url=connection_string)
files= mongo(collection="fichiers", db="vigiechiro", url=connection_string)
taxa= mongo(collection="taxons", db="vigiechiro", url=connection_string)
HowManyP=6
#test=sites$export()
SpeciesT="Pipnat"
ScoreT=0.5

Sys.time() #
alldatapart<-participations$find(fields='{}')
Sys.time() #
alldatataxa<-taxa$find(fields='{}')
Sys.time() #

SpN=match(SpeciesT,alldatataxa$libelle_court)
CodeSpT=alldatataxa$'_id'[SpN]

alldatapart=alldatapart[order(alldatapart$traitement$date_fin,decreasing = T),]

Found=vector()
i=0
while(length(Found)<HowManyP)
{
  i=i+1
  print(alldatapart$'_id'[i])
  
  datai=donnee_obs$find(query=paste0('{"participation":{"$oid":"',alldatapart$'_id'[i],'"}}'))
  ListObs=list()
  if(nrow(datai)>0){
    for (k in 1:nrow(datai)){
      if(length(datai$observations[[k]])>0){
        if(nrow(datai$observations[[k]])>0){
          ListObs[[k]]=datai$observations[[k]]
          ListObs[[k]]$tadarida_taxon_autre=NULL
          ListObs[[k]]$donnee=datai$titre[[k]]
        }
      }
    }
  }
    DataObsj=rbindlist(ListObs,use.names=T,fill=T)
  
  
  dataobsp=subset(DataObsj,DataObsj$tadarida_taxon==CodeSpT)
if(nrow(dataobsp)>0){
  print(nrow(dataobsp))
  print(max(dataobsp$tadarida_probabilite))
  if(max(dataobsp$tadarida_probabilite)>ScoreT){
    filesi=files$find(query=paste0('{"lien_participation":{"$oid":"',alldatapart$'_id'[i],'"}}'))
    filewi=subset(filesi,filesi$mime=="audio/wav")
    if(nrow(filewi)>nrow(filesi)/4)
    {
    print("wav")
      Found=c(Found,alldatapart$'_id'[i])
    
    }
  }
    
  }
  }
beep()

DataF=data.frame(Found)

fwrite(DataF,paste0("DataF_",SpeciesT[1],Sys.Date(),".csv"),sep=";")
