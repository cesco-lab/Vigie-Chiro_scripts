library(mongolite)
library(data.table)
library(beepr)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(uuid)
library(jsonlite)
library(rjson)


mongo=fread("C:/Users/yvesb/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/mongos.txt"
            ,sep="$",h=F)
test=F #T si base de test, F si base de prod
#id_observateur="5e9886c590250e001113d95d" #VC mnhn
#id_observateur="558acd059dcbdc000e0793ee" #Yves Bas
TriSite=T
RefSite="57b1f510d7d4b4000df26578"
TriPoint=T
ListPoint="Z2"
TriCoord=F
LongInterval=c(1.76,2.32)
LatInterval=c(42.40,42.76)
Tag="pourLucas"
DirOut="C:/Users/yvesb/Documents/VigieChiro/Exports/"
DetailProba=T #T if you want detailed probability of each class/species


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
donnee_obs = mongo(collection="donnees", db="vigiechiro", url=connection_string)
taxa= mongo(collection="taxons", db="vigiechiro", url=connection_string)

#test=sites$export()

Sys.time()
alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
Sys.time() #~1sec / 1e3 sites
alldatapart<-participations$find(fields='{}')

alldatataxa<-taxa$find(fields='{}')


test=sites$find(query = '{"titre" : "Vigiechiro - Point Fixe-340818"}',fields='{}') 


sllist=list()
for (i in 1:nrow(alldatasites)){
  if(!is.null(alldatasites$localites[[i]])){
    sllist[[i]]=as.data.table(alldatasites$localites[[i]])
    sllist[[i]]$longitude=alldatasites$localites[[i]]$geometries$geometries[[1]]$coordinates[[1]][2]
    sllist[[i]]$latitude=alldatasites$localites[[i]]$geometries$geometries[[1]]$coordinates[[1]][1]
    sllist[[i]]$titre=alldatasites$titre[i]
    row.names(sllist[[i]])=c()
    print(i)
    sllist[[i]]$idsite=alldatasites$'_id'[i]
  }
}
SiteLoc=rbindlist(sllist,use.names=T,fill=T)

Sys.time()

if(TriCoord)
{
  SiteLoc=subset(SiteLoc,(SiteLoc$longitude>LongInterval[1])
                 &(SiteLoc$longitude<LongInterval[2])
                 &(SiteLoc$latitude>LatInterval[1])
                 &(SiteLoc$latitude<LatInterval[2]))
}


if(TriSite)
{
  SiteLoc=subset(SiteLoc,SiteLoc$idsite %in% RefSite)
}


ListSite=unique(SiteLoc$idsite)

PartSel=subset(alldatapart,alldatapart$site %in% ListSite)

if(TriPoint)
{
  PartSel=subset(PartSel,PartSel$point %in% ListPoint)
}


ObsAlllist=list()
DetailAlllist=list()
for (j in 1:length(PartSel$'_id'))
{
  print(Sys.time())
  print(paste(j,"/",length(PartSel$'_id')))
  Obsj<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',PartSel$'_id'[j],'"}}'))
  ListObs=list()
  ListDetail=list()
  if(nrow(Obsj)>0){
    for (k in 1:nrow(Obsj)){
      if(length(Obsj$observations[[k]])>0){
        if(nrow(Obsj$observations[[k]])>0){
          #stop()
          if(DetailProba){
            ListDetail[[k]]=Obsj$observations[[k]]$tadarida_taxon_autre[[1]]
            ListDetail[[k]][nrow(ListDetail[[k]])+1,]=c(Obsj$observations[[k]]$tadarida_taxon
                                                        ,Obsj$observations[[k]]$tadarida_probabilite)
            ListDetail[[k]]$donnee=Obsj$titre[[k]]
            
          }
          ListObs[[k]]=Obsj$observations[[k]]
          ListObs[[k]]$tadarida_taxon_autre=NULL
          ListObs[[k]]$donnee=Obsj$titre[[k]]
        }
      }
    }
  }
  DataObsj=rbindlist(ListObs,use.names=T,fill=T)
  if(DetailProba){
    DataDetailj=rbindlist(ListDetail,use.names=T,fill=T)
    DetailAlllist[[j]]=DataDetailj
  }
  DataObsj$participation=PartSel$'_id'[j]
  ObsAlllist[[j]]=DataObsj
  
}

ObsAll=rbindlist(ObsAlllist,use.names=T,fill=T)

matchTaxa=match(ObsAll$tadarida_taxon,alldatataxa$'_id')

summary(matchTaxa)
test=subset(ObsAll,is.na(matchTaxa))

ObsAll$espece=alldatataxa$libelle_court[matchTaxa]
Missing=subset(ObsAll$tadarida_taxon,!(ObsAll$tadarida_taxon %in% alldatataxa$'_id'))
print(length(Missing))

table(ObsAll$espece)

fwrite(ObsAll,paste0(DirOut,"/Obs_",Tag,"_",Sys.Date(),".csv"),sep=";")

if(DetailProba){
DetailAll=rbindlist(DetailAlllist,use.names=T,fill=T)
matchTaxa=match(DetailAll$taxon,alldatataxa$'_id')

summary(matchTaxa)
test=subset(ObsAll,is.na(matchTaxa))

DetailAll$espece=alldatataxa$libelle_court[matchTaxa]

DetailCast=dcast(DetailAll,donnee~espece,fun.aggregate=function(x) paste(x, collapse="")
                 ,value.var ="probabilite")
fwrite(DetailCast,paste0(DirOut,"/Detail_",Tag,"_",Sys.Date(),".csv"),sep=";")

}


