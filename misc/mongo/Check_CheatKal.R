library(mongolite)
library(data.table)
library(beepr)
library(uuid)
library(jsonlite)


f2pPF <- function(x) #get date-time data from recording file names
{
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 21, nchar(x)-7), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-0
}


connection_string = "mongodb://vigiechiro:4Zp9nh8d2YSVt5@ccmg01.in2p3.fr:27030,ccmg02.in2p3.fr:27030,ccmg03.in2p3.fr:27030/vigiechiro?replicaSet=rs0&readPreference=secondaryPreferred&socketTimeoutMS=6000000" #site de prod
#connection_string = "mongodb://vigiechiro:kafc2xdr8vZdBpuV@ccdbmgtstno01.in2p3.fr:27080,ccdbmgtstno03.in2p3.fr:27080,ccdbmgtstno02.in2p3.fr:27080/vigiechiro?replicaSet=rs0&authSource=vigiechiro&socketTimeoutMS=6000000" #site de test
fichiers = mongo(collection="fichiers", db="vigiechiro", url=connection_string)
#participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
#users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)
AllEcarts=fread("C:/Users/yvesb/Documents/VigieChiro/Raw/AllEcarts.csv")
#test=fichiers$export()


CheatKalPot=subset(AllEcarts,AllEcarts$CheatKal)


FilList=list()
for (i in 1:nrow(CheatKalPot)){
  print(Sys.time())
  print(CheatKalPot$Var1[i])
  print(i)
  files_parti <- fichiers$find(query=paste0('{"lien_participation" : {"$oid":"',CheatKalPot$Var1[i],'"}}')
                               ,fields='{"titre":true}')
  Sys.time() #~1sec / 1e3 sites
  FilList[[i]]=files_parti
  
}

filesAll=rbindlist(FilList)

filesAll$time=f2pPF(filesAll$titre)
filesAll$DecTime=c(0,filesAll$time[2:nrow(filesAll)]-filesAll$time[1:(nrow(filesAll)-1)])
filesAll$DecTimeF=floor(filesAll$DecTime)
table(filesAll$DecTimeF)[order(table(filesAll$DecTimeF),decreasing=T)][1:20]

filesAll30=subset(filesAll,filesAll$DecTimeF<=30&filesAll$DecTimeF>=0)
StatEcart=as.data.frame(table(filesAll30$participation,filesAll30$DecTimeF))
StatEcart=dcast(StatEcart,"Var1~Var2")
EcartList[[i]]=StatEcart



Sys.time()
alldatapart<-participations$find(fields='{}')
Sys.time()
alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
Sys.time() #~1sec / 1e3 sites



Seldatapart=subset(alldatapart,alldatapart$'_id' %in% listpart$participation)

testS=match(Seldatapart$site,alldatasites$'_id')
Seldatapart$numcarre=alldatasites$titre[testS]

tail(Seldatapart$numcarre,200)
tail(Seldatapart$'_id',200)


NewCoordGI=MetadataBMRE
coordinates(NewCoordGI)=CoordNames
proj4string(NewCoordGI) <- CRS("+init=epsg:4326")
NewCoordL2E=spTransform(NewCoordGI,CRS("+init=epsg:27572"))

Sys.time()
#SiteLoc=as.data.frame(do.call(rbind,alldatasites$localites))


sllist=list()
for (i in 1:nrow(alldatasites)){
  if(!is.null(alldatasites$localites[[i]])){
    #if(grepl("620981",alldatasites$titre[i])){stop()}
    sllist[[i]]=as.data.table(alldatasites$localites[[i]])
    coordi=rbindlist(alldatasites$localites[[i]]$geometries$geometries,use.names=T)
    coordi2=t(as.data.frame(coordi$coordinates))
    sllist[[i]]$longitude=coordi2[,2]
    sllist[[i]]$latitude=coordi2[,1]
    sllist[[i]]$titre=alldatasites$titre[i]
    sllist[[i]]$idsite=alldatasites$'_id'[i]
    row.names(sllist[[i]])=c()
    if(i%%1000==1){print(paste(i,Sys.time()))}
  }
}
SiteLoc=rbindlist(sllist,use.names=T,fill=T)

test=subset(SiteLoc,grepl("620981",SiteLoc$titre))

Sys.time()

SiteLocGI=SiteLoc
coordinates(SiteLocGI)=c("longitude","latitude")
proj4string(SiteLocGI) <- CRS("+init=epsg:4326")
SiteLocL2E=spTransform(SiteLocGI,CRS("+init=epsg:27572"))




Sys.time()
testNN=gDistance(NewCoordL2E,SiteLocL2E,byid=TRUE) # 1 sec / 6e3 squares
Sys.time()

ClosestN=apply(testNN,2,min)
summary(ClosestN)
MissingPoints=subset(MetadataBMRE,ClosestN>30)
if(max(ClosestN)>30){
  fwrite(MissingPoints,paste0("MissingPoints",Sys.Date(),".csv"),sep=";")
  stop("points manquants !!")
}


NumNN=apply(testNN,2,which.min)

#NewCarre=as.character(sprintf("%06d",GridStoc$NUMNAT[NumNN]))
NewCarre=SiteLoc$titre[NumNN]
table(NewCarre)
MetadataBMRE$Carre=NewCarre
MetadataBMRE$idsite=SiteLoc$idsite[NumNN]
MetadataBMRE$Point=SiteLoc$nom[NumNN]


#for (z in 30000:30010){
 # print(z)
  #print(alldatapart$bilan[z,])
  
#}


#NewSq=vector()
TemplateAddData=alldatapart[1,]
TemplateAddData$'_id'=NULL
TemplateAddData$meteo$couverture=NA
TemplateAddData$meteo$temperature_debut=NA
TemplateAddData$meteo$temperature_fin=NA
TemplateAddData$meteo$vent=NA
TemplateAddData$configuration$micro0_position=NA
TemplateAddData$configuration$detecteur_enregistreur_numero_serie=NA
TemplateAddData$configuration$micro0_numero_serie=NA
TemplateAddData$configuration$micro1_position=NA
TemplateAddData$configuration$micro1_numero_serie=NA
TemplateAddData$configuration$micro1_hauteur=NA
TemplateAddData$configuration$canal_expansion_temps=NA
TemplateAddData$configuration$canal_enregistrement_direct=NA
TemplateAddData$traitement$etat=NA
TemplateAddData$traitement$date_debut=NA
TemplateAddData$traitement$date_fin=NA
TemplateAddData$traitement$message=NA
TemplateAddData$logs=NA
TemplateAddData$bilan$problemes=NA
TemplateAddData$bilan$chiropteres=NULL
TemplateAddData$bilan$orthopteres=NULL
TemplateAddData$bilan$autre=NULL
TemplateAddData$messages=NULL
#tester la grille repr?sentative
MetadataBMRE$idparticipation=""
for (i in 1:nrow(MetadataBMRE))
{
  print(i)
  NewData=TemplateAddData
  while(NewData$'_etag' %in% alldatapart$'_etag')
  {
    NewEtag=UUIDgenerate()
    NewEtag=gsub("-","",NewEtag)
    NewData$'_etag'=NewEtag
  }
  test=match(MetadataBMRE$Recorder[i],CorrRecorders$Code)
  NewData$configuration$detecteur_enregistreur_type=CorrRecorders$Recorder[test]  
  test=match(MetadataBMRE$Mic[i],CorrMics$Code)
  NewData$configuration$micro0_type=CorrMics$Mic[test]
  NewData$configuration$micro0_hauteur=MetadataBMRE$MicHeight[i]
  NewData$'_created'=Sys.time()
  NewData$'_updated'=Sys.time()
  D1=paste0(substr(MetadataBMRE$StartDate[i],7,10),"/",substr(MetadataBMRE$StartDate[i],4,5),"/"
            ,substr(MetadataBMRE$StartDate[i],1,2)," 12:00:00 CEST")
  NewData$date_debut=as.POSIXct(D1)
  #NewData$date_debut=paste0(MetadataBMRE$StartDate[i]," 12:00:00 CEST")
  D2=paste0(substr(MetadataBMRE$EndDate[i],7,10),"/",substr(MetadataBMRE$EndDate[i],4,5),"/"
            ,substr(MetadataBMRE$EndDate[i],1,2)," 12:00:00 CEST")
  NewData$date_fin=as.POSIXct(D2)
  NewData$point=MetadataBMRE$Point[i]
  NewData$commentaire=MetadataBMRE$Comment[i]
  participations$insert(NewData)
  
  #add MongoId via Json syntax
  test=participations$find(paste0('{"_etag":"',NewData$'_etag','"}'),fields='{}')
  
  #ExpParti=capture.output(sites$export(query = paste0('{"_etag":"',NewData$'_etag','"}')))
  
  #participations$remove(paste0('{"_etag":"',NewData$'_etag','"}'))
  participations$update(paste0('{"_etag":"',NewData$'_etag','"}')
                        , paste0('{"$set":{"protocole":{ "$oid" : "54bd090f1d41c8103bad6252" }}}')) #protocole
  
  Obsi=users$find(query=paste0('{"email":"',MetadataBMRE$Email[i],'"}'),fields='{}')
  id_observateur= Obsi$'_id'
  
  participations$update(paste0('{"_etag":"',NewData$'_etag','"}')
                        , paste0('{"$set":{"observateur":{ "$oid" : "',id_observateur,'" }}}')) #observateur
  
  participations$update(paste0('{"_etag":"',NewData$'_etag','"}')
                        , paste0('{"$set":{"site":{ "$oid" : "',MetadataBMRE$idsite[i],'" }}}')) #site

MetadataBMRE$idparticipation=test$'_id'[1]    
}

fwrite(MetadataBMRE,gsub(".csv","_created.csv",FileBMRE),sep=";")
