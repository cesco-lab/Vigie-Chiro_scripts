library(mongolite)
library(data.table)
library(beepr)
library(raster)
library(sf)
library(uuid)
library(jsonlite)
library(rjson)


mongo=fread("mongos.txt",sep="$",h=F) #hack
test=T #T si base de test, F si base de prod



if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}




sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)

#test=sites$export()


Sys.time()
alldatasites <- sites$find(fields='{}') #protocole PF
Sys.time() #~1sec / 1e3 sites

test=sites$find(query = '{"titre" : "Vigiechiro - Point Fixe-340818"}',fields='{}') 
sllist=list()
for (i in 1:nrow(alldatasites)){
  if(!is.null(alldatasites$localites[[i]])){
    sllist[[i]]=as.data.table(alldatasites$localites[[i]])
    if(alldatasites$protocole[i]=="54bd08ba1d41c8103bad6251"){ #cas particulier pour routier
#      stop()
      
      Lats=vector()
      Longs=vector()
      for (j in 1:nrow(sllist[[i]])){
        Lats[j]=mean(alldatasites$localites[[i]]$geometries$geometries[[j]]$coordinates[[1]][,1])
        Longs[j]=mean(alldatasites$localites[[i]]$geometries$geometries[[j]]$coordinates[[1]][,2])
      }
      sllist[[i]]$longitude=Longs
      sllist[[i]]$latitude=Lats
    }else{
      for (x in 1:nrow(sllist[[i]])){
      sllist[[i]]$longitude[x]=alldatasites$localites[[i]]$geometries$geometries[[x]]$coordinates[[1]][2]
      sllist[[i]]$latitude[x]=alldatasites$localites[[i]]$geometries$geometries[[x]]$coordinates[[1]][1]
      }
    }
    
    sllist[[i]]$site=alldatasites$titre[i]
    sllist[[i]]$id_protocole=alldatasites$protocole[i]
    sllist[[i]]$id_site=alldatasites$'_id'[i]
    sllist[[i]]$commentaire=alldatasites$commentaire[i]
    sllist[[i]]$'_created'=alldatasites$'`_created'[i]
    row.names(sllist[[i]])=c()
    print(i)
  }
}
SiteLoc=rbindlist(sllist,use.names=T,fill=T)

SiteLoc$protocole=ifelse(SiteLoc$id_protocole=="54bd090f1d41c8103bad6252"
                         ,"POINT_FIXE",ifelse(
                           SiteLoc$id_protocole=="54bd08ba1d41c8103bad6251"
                           ,"ROUTIER","CARRE"
                         ))

SiteR=subset(SiteLoc,SiteLoc$protocole=="ROUTIER")

table(SiteLoc$protocole)
SiteLoc$geometries.type=NULL
SiteLoc$geometries.geometries=NULL

SiteLoc=subset(SiteLoc,!is.na(as.numeric(SiteLoc$longitude)))

#fwrite(SiteLoc,"sites_localites.csv",sep=";")

Sys.time()

test=subset(SiteLoc,SiteLoc$site=="Vigiechiro - Point Fixe-BY051365")

Gite=mapply(function(x,y) 
  ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
  ,SiteLoc$commentaire
  ,SiteLoc$nom)

summary(Gite)

SiteLoc$Gite=Gite
fwrite(SiteLoc,"sites_localites.txt",sep=";")
SiteLocPF=subset(SiteLoc,SiteLoc$protocole=="POINT_FIXE")
SiteLocRP=subset(SiteLoc,SiteLoc$protocole!="POINT_FIXE")
SiteLocRP=subset(SiteLocRP,(!grepl("T",SiteLocRP$nom))|grepl("3",SiteLocRP$nom))
fwrite(SiteLocPF,"sites_localites_PF.txt",sep=";")


test104=subset(alldatasites,alldatasites$observateur=="6a2d70a7495e9e1dbdc4abae")
test105=subset(alldatasites,alldatasites$observateur=="6a2edb37b2f35af2be3ccd47")
