### ATTENTION SCRIPT TRES DANGEREUX / A MANIER AVEC BEAUCOUP BEAUCOUP DE PRECAUTIONS !!!!


library(mongolite)
library(data.table)
library(beepr)
library(jsonlite)

mongo=fread("mongos.txt",sep="$",h=F)
test=F
# ToSuppress=data.frame(p="58fe0beabc0b3f000e89f628")
# ToSuppress=fread("ToSuppress230208.csv",h=F)
# SelSite=T
# ListSite=c("230502","230543","230544")

if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
#listpart=fread("ccsps220906.csv")
#test=participations$export()

Sys.time()
alldatasites<-sites$find(fields='{}')
Sys.time()

if(SelSite){
for (a in 2:length(ListSite)){
  sitesa=subset(alldatasites,grepl(paste0("Fixe-",ListSite[a]),alldatasites$titre))
  if(nrow(sitesa)!=1){stop("site non unique")}
  test=  participations$find(query=paste0('{"site":{"$oid":"',sitesa$`_id`,'"}}'))
  participations$remove(query=paste0('{"site":{"$oid":"',sitesa$`_id`,'"}}'))  
  
}

  
  
    
}else{
  
  names(ToSuppress)="V1"
  LPS=ToSuppress$V1
  
  for (i in 1:length(LPS)){
    #test=  participations$find(query=paste0('{"_id":{"$oid":"',LPS[i],'"}}'))
    print(LPS[i])
    participations$remove(query=paste0('{"_id":{"$oid":"',LPS[i],'"}}'))  
  }
}
