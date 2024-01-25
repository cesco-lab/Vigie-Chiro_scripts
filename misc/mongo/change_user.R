library(mongolite)
library(data.table)
library(beepr)
library(uuid)
library(jsonlite)
library(lubridate)


mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
SelU=F
FromU="5b3253e1c4e03f000f7bda50"
SelS=T
FromS=c("62d42cada5c4f02cfd8b441d","62d42ea8df3b3e229f44177a","62d42e21a5afdbb00fe7dc2c")
ToU="642fe61862f121f5049cbab5"

if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}


#queuer_jobs = mongo(collection="queuer_jobs", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
sites = mongo(collection="sites", db="vigiechiro", url=connection_string)


Sys.time()
alldatasites<-sites$find(fields='{}')
Sys.time()
alldataobs<-users$find(fields='{}')
Sys.time()

test=subset(alldataobs,alldataobs$email=="vigiechiro49@gmail.com")


if(SelU){
  SiteSel=sites$find(query=paste0('{"observateur":{"$oid":"',FromU,'"}}'),fields='{}')
  print(dim(SiteSel))
  
  SiteN=sites$find(query=paste0('{"observateur":{"$oid":"',ToU,'"}}'),fields='{}')
  
  print(dim(SiteN))
  
  
  sites$update(query=paste0('{"observateur":{"$oid":"',FromU,'"}}'),paste0('{"$set":{"observateur": {"$oid":"'
                                                                           ,ToU,'"}}}'),multiple=T)
  
  SiteSel=sites$find(query=paste0('{"observateur":{"$oid":"',FromU,'"}}'),fields='{}')
  
  print(dim(SiteSel))
  
  SiteN=sites$find(query=paste0('{"observateur":{"$oid":"',ToU,'"}}'),fields='{}')
  
  print(dim(SiteN))
  
}

if(SelS){
  #SiteSel=subset(alldatasites,alldatasites$'_id'==FromS) 
  
  for (s in 1:length(FromS)){
    
    SiteSel=sites$find(query=paste0('{"_id":{"$oid":"',FromS[s],'"}}'),fields='{}')
    
    print(dim(SiteSel))
    
    SiteN=sites$find(query=paste0('{"observateur":{"$oid":"',ToU,'"}}'),fields='{}')
    
    print(dim(SiteN))
    
    sites$update(query=paste0('{"_id":{"$oid":"',FromS[s],'"}}'),paste0('{"$set":{"observateur": {"$oid":"'
                                                                     ,ToU,'"}}}'),multiple=T)
    
    SiteSel=sites$find(query=paste0('{"_id":{"$oid":"',FromS[s],'"}}'),fields='{}')
    
    print(dim(SiteSel))
    
    SiteN=sites$find(query=paste0('{"observateur":{"$oid":"',ToU,'"}}'),fields='{}')
    
    print(dim(SiteN))
    
  }
}



