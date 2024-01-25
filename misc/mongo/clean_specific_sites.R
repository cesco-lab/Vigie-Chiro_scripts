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

TitleToSuppress="980086"
MaxToSuppress=2


sites = mongo(collection="sites", db="vigiechiro", url=connection_string)

Sys.time()
alldatasites <- sites$find(fields='{}') 
Sys.time() #~1sec / 1e3 sites


test=sites$find(query='{"_id":{"$oid":"63b6e9977372222372059f01"}}')

sitesSelected=subset(alldatasites,grepl(TitleToSuppress,alldatasites$titre))

if(nrow(sitesWithoutParticipant)<=MaxToSuppress){
  for (i in 1:nrow(sitesSelected)){
    
    sites$remove(query=paste0('{"_id":{"$oid":"',sitesSelected$'_id'[i],'"}}'))
  }
  
}
