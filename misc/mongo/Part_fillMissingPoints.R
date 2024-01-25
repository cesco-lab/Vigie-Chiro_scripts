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



mongo=fread("C:/Users/Yves Bas/Desktop/mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
donnee_obs = mongo(collection="donnees", db="vigiechiro", url=connection_string)
#taxa= mongo(collection="taxons", db="vigiechiro", url=connection_string)

#test=sites$export()

Sys.time()
alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
Sys.time() #~1sec / 1e3 sites
alldatapart<-participations$find(fields='{}')

#alldatataxa<-taxa$find(fields='{}')




# sllist=list()
# for (i in 1:nrow(alldatasites)){
#   if(!is.null(alldatasites$localites[[i]])){
#     sllist[[i]]=as.data.table(alldatasites$localites[[i]])
#     sllist[[i]]$longitude=alldatasites$localites[[i]]$geometries$geometries[[1]]$coordinates[[1]][2]
#     sllist[[i]]$latitude=alldatasites$localites[[i]]$geometries$geometries[[1]]$coordinates[[1]][1]
#     sllist[[i]]$titre=alldatasites$titre[i]
#     row.names(sllist[[i]])=c()
#     print(i)
#     sllist[[i]]$idsite=alldatasites$'_id'[i]
#   }
# }
# SiteLoc=rbindlist(sllist,use.names=T,fill=T)



PartSel=subset(alldatapart,is.na(alldatapart$point)
               &(alldatapart$protocole=="54bd090f1d41c8103bad6252"))


#ObsAlllist=list()
for (j in 1:length(PartSel$'_id'))
{
  print(Sys.time())
  print(j)
  Obsj<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',PartSel$'_id'[j],'"}}'))
  print(Sys.time())
  if(nrow(Obsj)>0){
    Info=tstrsplit(Obsj$titre[1],split="-")
    if(length(Info)>4){
      Pointj=Info[[4]]
      print(Pointj)
      Sitesj=subset(alldatasites,alldatasites$'_id'==PartSel$site[j])
      PointExistants=Sitesj$localites[[1]]$nom
      if(Pointj %in% PointExistants){
        #test=participations$find(query =paste0('{"_id":{"$oid":"'
        #                                      ,PartSel$'_id'[j],'"}}'))
        #test=participations$export()
        participations$update(query =paste0('{"_id":{"$oid":"'
                                            ,PartSel$'_id'[j],'"}}')
                              ,paste0('{"$set":{"point" : "',Pointj,'" }}'))
        print("done")
      }
    }
  }
  
}

