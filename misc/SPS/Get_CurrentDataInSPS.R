library(data.table)
library(mongolite)


CCSPS=fread("ccsps230419.txt")
mongo=fread("mongos.txt",sep="$",h=F)
test=F #T si base de test, F si base de prod

if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
#listpart=fread("ccsps220906.csv")
#test=participations$export()
users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)


Sys.time()
alldatapart<-participations$find(fields='{}') # ~1 sec / 1e3 participations
Sys.time()
alldatasites <- sites$find(fields='{}')
Sys.time() #~1sec / 1e3 sites
alldataobs <- users$find(fields='{}')
Sys.time()

obsopen=subset(alldataobs,alldataobs$donnees_publiques)

CCsel=subset(CCSPS,grepl("vigiechiro-prod-datastore",CCSPS$'Directory or file in "/sps/mnhn"'))
CCsel=subset(CCsel,grepl("GiB",CCsel$`Space used`))
CCsel$participation=gsub("vigiechiro/vigiechiro-prod-datastore/","",CCsel$'Directory or file in "/sps/mnhn"')

testP=match(CCsel$participation,alldatapart$'_id')
CCsel$site=alldatapart$site[testP]
CCsel$point=alldatapart$point[testP]
CCsel$user=alldatapart$observateur[testP]
CCsel$date=alldatapart$date_debut[testP]
CCsel$enregistreur=alldatapart$configuration$detecteur_enregistreur_type[testP]
CCsel$micro=alldatapart$configuration$micro0_type[testP]

  
CCsel=subset(CCsel,CCsel$user %in% obsopen$'_id')

for (i in 1:nrow(CCsel)){
  if(i%%1000==1){print(i)}
  Sitei=subset(alldatasites,alldatasites$'_id'==CCsel$site[i])  
  CCsel$latitude[i]=Sitei$localites[[1]]$geometries$geometries[[1]]$coordinates[[1]][1]
  CCsel$longitude[i]=Sitei$localites[[1]]$geometries$geometries[[1]]$coordinates[[1]][2]
  CCsel$carre[i]=Sitei$titre
}

fwrite(CCsel,paste0("CCsel_",Sys.Date(),".csv"),sep=";")



