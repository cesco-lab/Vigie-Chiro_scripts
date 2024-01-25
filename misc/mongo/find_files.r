library(mongolite)
library(data.table)
library(beepr)
library(uuid)
library(jsonlite)



connection_string = "mongodb://vigiechiro:4Zp9nh8d2YSVt5@ccmg01.in2p3.fr:27030,ccmg02.in2p3.fr:27030,ccmg03.in2p3.fr:27030/vigiechiro?replicaSet=rs0&readPreference=secondaryPreferred&socketTimeoutMS=6000000" #base de prod
#connection_string = "mongodb://vigiechiro:kafc2xdr8vZdBpuV@ccdbmgtstno01.in2p3.fr:27080,ccdbmgtstno03.in2p3.fr:27080,ccdbmgtstno02.in2p3.fr:27080/vigiechiro?replicaSet=rs0&authSource=vigiechiro&socketTimeoutMS=6000000" #base de test
#MetadataBMRE=fread("C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table.csv")
MetadataBMRE=fread("C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table_bis_corrected.csv")
#id_observateur="5e9886c590250e001113d95d" #VC mnhn
#id_observateur="558acd059dcbdc000e0793ee" #Yves Bas
fichiers = mongo(collection="fichiers", db="vigiechiro", url=connection_string)
#sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
#grille= mongo(collection="grille_stoc", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
Part=c("64bf981984d243decc0e780d")

#MetadataBMRE=subset(MetadataBMRE,MetadataBMRE$Participation %in% Part)


if(!is.na(Part)){
  MetadataBMRE=data.frame(idparticipation=Part)
  
  
}
#test=fichiers$export()

FilList=list()
for (i in 1:nrow(MetadataBMRE)){
  print(Sys.time())
  print(MetadataBMRE$idparticipation[i])
  print(i)
  files_parti <- fichiers$find(query=paste0('{"lien_participation" : {"$oid":"',MetadataBMRE$idparticipation[i],'"}}')
                               ,fields='{"titre":true}')
  files_parti$participation=MetadataBMRE$idparticipation[i]
  Sys.time() #~1sec / 1e3 sites
print(nrow(files_parti))
    FilList[[i]]=files_parti
  
}




AllFiles=rbindlist(FilList)



