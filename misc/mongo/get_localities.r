library(mongolite)
library(data.table)
library(beepr)
library(raster)
library(sf)
library(uuid)
library(jsonlite)
library(rjson)


mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
#MetadataBMRE=fread("C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table.csv")
#MetadataBMRE=fread("MissingPoints2022-07-19.csv") #table avec les points à créer
#CoordNames=c("xcoord","ycoord")


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
    sllist[[i]]$longitude=alldatasites$localites[[i]]$geometries$geometries[[1]]$coordinates[[1]][2]
    sllist[[i]]$latitude=alldatasites$localites[[i]]$geometries$geometries[[1]]$coordinates[[1]][1]
    sllist[[i]]$site=alldatasites$titre[i]
    sllist[[i]]$id_protocole=alldatasites$protocole[i]
    sllist[[i]]$id_site=alldatasites$'_id'[i]
    sllist[[i]]$commentaire=alldatasites$commentaire[i]
    
    
    
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

table(SiteLoc$protocole)
SiteLoc$geometries.type=NULL
SiteLoc$geometries.geometries=NULL

fwrite(SiteLoc,"sites_localites.txt",sep=";")

Sys.time()

