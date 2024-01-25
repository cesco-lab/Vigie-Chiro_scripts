library(mongolite)
library(data.table)
library(beepr)
library(uuid)
library(jsonlite)


mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
MetadataBMRE=fread("C:/Users/ybas/Downloads/Disque_terrain_1.csv") #table avec les points à créer
#id_observateur="5e9886c590250e001113d95d" #VC mnhn
DataPublic=T
Charte=F

if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


UsersToImport=unique(MetadataBMRE,by="Email")

users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
#grille= mongo(collection="grille_stoc", db="vigiechiro", url=connection_string)

#test=sites$export()


Sys.time()
alldatausers <- users$find(query='{}')#,fields='{}') 
Sys.time() #~1sec / 1e3 sites

for (i in 1:nrow(UsersToImport)){
  #test=sites$find(query = '{"titre" : "Vigiechiro - Point Fixe-340818"}',fields='{}') 
  NewUser=alldatausers[1,]
  NewUser$'_updated'=Sys.time()
  NewUser$'_created'=Sys.time()
  NewUser$donnees_publiques=DataPublic
  NewUser$email=UsersToImport$Email[i]
  print(NewUser$email)
  NewUser$tokens=NA
  while(NewUser$'_etag' %in% alldatausers$'_etag')
  {
    NewEtag=UUIDgenerate()
    NewEtag=gsub("-","",NewEtag)
    NewUser$'_etag'=NewEtag
  }
  NewUser$google_id=NA
  NewUser$pseudo=paste(UsersToImport$FirstName[i],UsersToImport$FamilyName[i])
  NewUser$role="Observateur"
  NewUser$actualites_suivies=NA
  NewUser$protocoles=NA
  NewUser$professionel=NA
  NewUser$nom=UsersToImport$FamilyName[i]
  NewUser$prenom=UsersToImport$FirstName[i]
  NewUser$adresse=NA
  NewUser$telephone=NA
  NewUser$organisation=UsersToImport$Affiliation[i]
  NewUser$charte_acceptee=Charte
  NewUser$facebook_id=NA
  NewUser$commentaire=NA
  NewUser$professionel=NA
users$insert(NewUser)
  }

