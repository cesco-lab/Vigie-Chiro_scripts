library(mongolite)
library(data.table)
library(beepr)
library(uuid)
library(jsonlite)
library(rjson)


mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod

if(test){
    connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}




sites = mongo(collection="sites", db="vigiechiro", url=connection_string)

Sys.time()
alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
Sys.time() #~1sec / 1e3 sites


test=sites$find(query='{"_id":{"$oid":"63a176de46bb6e924d09d511"}}')

sitesWithoutParticipant=subset(alldatasites,is.na(alldatasites$observateur))

if(nrow(sitesWithoutParticipant)==1){
  sites$remove(query=paste0('{"_id":{"$oid":"',sitesWithoutParticipant$'_id','"}}'))
}

