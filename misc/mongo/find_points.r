library(mongolite)
library(data.table)
library(beepr)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(uuid)
library(jsonlite)
library(rjson)


mongo=fread("mongos.txt",sep="$",h=F) #hack
test=F #T si base de test, F si base de prod
#MetadataBMRE=fread("C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table.csv")
#MetadataBMRE=fread("MissingPoints2022-07-19.csv") #table avec les points à créer
MetadataBMRE=fread("C:/Users/yvesb/Downloads/Fiche_terrain_COS_WGS84_pourVigieChiro.csv") #table avec les points à créer
ParticipanteSpecified="tiphainedevaux@gmail.com"
ToleranceDoublon=40

CoordNames=c("X","Y")
CoordNames=c("xcoord","ycoord")


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}




sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
grille= mongo(collection="grille_stoc", db="vigiechiro", url=connection_string)
users = mongo(collection="utilisateurs", db="vigiechiro", url=connection_string)

#test=sites$export()


Sys.time()
alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
Sys.time() #~1sec / 1e3 sites

test=sites$find(query = '{"titre" : "Vigiechiro - Point Fixe-340818"}',fields='{}') 
sllist=list()
for (i in 1:nrow(alldatasites)){
  if(!is.null(alldatasites$localites[[i]])){
    sllist[[i]]=as.data.table(alldatasites$localites[[i]])
    sllist[[i]]$longitude=alldatasites$localites[[i]]$geometries$geometries[[1]]$coordinates[[1]][2]
    sllist[[i]]$latitude=alldatasites$localites[[i]]$geometries$geometries[[1]]$coordinates[[1]][1]
    sllist[[i]]$titre=alldatasites$titre[i]
    row.names(sllist[[i]])=c()
    print(i)
  }
}
SiteLoc=rbindlist(sllist,use.names=T,fill=T)

Sys.time()

SiteLocGI=SiteLoc
coordinates(SiteLocGI)=c("longitude","latitude")
proj4string(SiteLocGI) <- CRS("+init=epsg:4326")
SiteLocL2E=spTransform(SiteLocGI,CRS("+init=epsg:27572"))



alldatagrid<-grille$find(fields='{}')
Sys.time() #~1min /4e5 squares
allcoords=as.data.frame(do.call(cbind, alldatagrid$centre$coordinates))   
Sys.time()
allcoordst=t(allcoords) #1 sec=2e5 squares
Sys.time()
allcoordst=as.data.frame(allcoordst)
Sys.time()
names(allcoordst)=c("longitude","latitude")
allcoordst$numero=alldatagrid$numero
allcoordst$id=alldatagrid$'_id'

#r?cup?rer les nouvelles coordonn?es
NewCoord=unique(MetadataBMRE,by=CoordNames)
testC=match(CoordNames,names(NewCoord))
names(NewCoord)[testC[1]]="X"
names(NewCoord)[testC[2]]="Y"
CoordOnly=subset(NewCoord,select=c("X","Y"))
if(ncol(CoordOnly)!=2){stop("bad coord names")}


#FILTRER la grille autour des sites
summary(CoordOnly)
allcoordst=subset(allcoordst,(allcoordst$longitude>(min(CoordOnly[,1])-0.1))
                  &(allcoordst$longitude<(max(CoordOnly[,1])+0.1))
                  &(allcoordst$latitude>(min(CoordOnly[,2])-0.1))
                  &(allcoordst$latitude<(max(CoordOnly[,2])+0.1)))


coordinates(allcoordst)=c("longitude","latitude")
proj4string(allcoordst) <- CRS("+init=epsg:4326")
CentroidsStoc=spTransform(allcoordst,CRS("+init=epsg:27572")) #L2E

#cr?er les grilles de points repr?sentatifs
StocA1=elide(CentroidsStoc, shift=c(-750,750))
StocA2=elide(CentroidsStoc, shift=c(-250,750))
StocB1=elide(CentroidsStoc, shift=c(250,750))
StocB2=elide(CentroidsStoc, shift=c(750,750))
StocC1=elide(CentroidsStoc, shift=c(-250,250))
StocC2=elide(CentroidsStoc, shift=c(-750,250))
StocD1=elide(CentroidsStoc, shift=c(750,250))
StocD2=elide(CentroidsStoc, shift=c(250,250))
StocE1=elide(CentroidsStoc, shift=c(-750,-250))
StocE2=elide(CentroidsStoc, shift=c(-250,-250))
StocF1=elide(CentroidsStoc, shift=c(250,-250))
StocF2=elide(CentroidsStoc, shift=c(750,-250))
StocG1=elide(CentroidsStoc, shift=c(-250,-750))
StocG2=elide(CentroidsStoc, shift=c(-750,-750))
StocH1=elide(CentroidsStoc, shift=c(750,-750))
StocH2=elide(CentroidsStoc, shift=c(250,-750))



NewCoordGI=NewCoord
coordinates(NewCoordGI)=c("X","Y")
proj4string(NewCoordGI) <- CRS("+init=epsg:4326")
NewCoordL2E=spTransform(NewCoordGI,CRS("+init=epsg:27572"))

Sys.time()
#SiteLoc=as.data.frame(do.call(rbind,alldatasites$localites))






#test 0 : points trop proches dans NewCoord ?
test=gDistance(NewCoordL2E, NewCoordL2E,byid=TRUE)  
testSansDiag=test[-seq(1,(nrow(test)^2),nrow(test)+1)]
Closest=min(testSansDiag)
print(Closest)
if(Closest<ToleranceDoublon){
  AllDoublons=subset(testSansDiag,testSansDiag<ToleranceDoublon)
  ValU=unique(AllDoublons)
  for (z in 1:length(ValU))
  {
    Doublon1=which(test==ValU[z],arr.ind=TRUE)
    PointsDoublon=as.data.frame(MetadataBMRE)[Doublon1,]
    print(PointsDoublon)
  }
  stop("points doublons dans input table")
}

 
#test 1 : point existant ? A RECODER EN UTILISANT coord en WGS84
test=gDistance(NewCoordL2E, SiteLocL2E,byid=TRUE)  
Closest=apply(test,2,min)
summary(Closest)


if(min(Closest)<=ToleranceDoublon)
{
  #stop("recoder point d?j? existant")
  Existing=subset(NewCoord,Closest<ToleranceDoublon)
  dim(Existing)
  Wclosest=apply(test,2,which.min)
  WclosestE=subset(Wclosest,Closest<ToleranceDoublon)
  Existing$titre=SiteLocL2E$titre[WclosestE]
  Existing$Point=SiteLocL2E$nom[WclosestE]
  NewCoordL2E=subset(NewCoordL2E,Closest>=ToleranceDoublon)
  NewCoord=subset(NewCoord,Closest>=ToleranceDoublon)
}else{
  Existing=NewCoordL2E[0,]
}

test2=gDistance(NewCoordL2E,NewCoordL2E,byid=TRUE)  
test2[test2==0]=999999
ClosestY=apply(test2,2,min)
summary(ClosestY)


if(min(ClosestY)<=ToleranceDoublon)
{
  stop("coder points doublons")
}

Sys.time()
testNN=gDistance(NewCoordL2E,CentroidsStoc,byid=TRUE) # 1 sec / 6e3 squares
Sys.time()

ClosestN=apply(testNN,2,min)
summary(ClosestN)
NumNN=apply(testNN,2,which.min)

#NewCarre=as.character(sprintf("%06d",GridStoc$NUMNAT[NumNN]))
NewCarre=ifelse(nchar(CentroidsStoc$numero[NumNN])==5
                ,paste0("0",CentroidsStoc$numero[NumNN])
                ,as.character(CentroidsStoc$numero[NumNN]))
#table(NewCarre)
NewCoord$Carre=NewCarre

table(NewCoord$Carre)
test=subset(NewCoord,NewCoord$Carre=="560669")

RefList=list(StocA1,StocA2,StocB1,StocB2,StocC1,StocC2,StocD1,StocD2
             ,StocE1,StocE2,StocF1,StocF2,StocG1,StocG2,StocH1,StocH2)
NameList=c("A1","A2","B1","B2","C1","C2","D1","D2"
           ,"E1","E2","F1","F2","G1","G2","H1","H2")


NewSq=vector()
TemplateAddData=alldatasites[1,]
TemplateAddData$'_id'=NULL
#tester la grille repr?sentative
for (i in 4:nrow(NewCoordL2E))
{
  print(i)
  #coordinates(NewCoordL2E[i,])
  Dist=vector()
  for (j in 1:length(RefList))
  {
    distj=gDistance(NewCoordL2E[i,],RefList[[j]][NumNN[i],])
    Dist=c(Dist,distj)
  }
  if(min(Dist)<30)
  {
    #stop("coder point représentatif de la grille")
    NewCoord$Point[i]=NameList[which.min(Dist)]
    Carre_existant1=subset(SiteLoc,SiteLoc$titre==paste0("Vigiechiro - Point Fixe-",NewCoord$Carre[i]))
    Carre_existant2=subset(SiteLoc,SiteLoc$titre==paste0("Vigiechiro - Point Fixe-0",NewCoord$Carre[i]))
    if((nrow(Carre_existant1)+nrow(Carre_existant2))>0)
    {
      #stop("recoder carre existant")
      Carrei=sites$find(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}'),fields='{}') 
      ExpCarrei=capture.output(sites$export(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')))
      
      test=unlist(gregexpr('"representatif" : false', ExpCarrei)) 
      test2=unlist(gregexpr('"representatif" : true', ExpCarrei)) 
      LastPoint=max(c(test+25,test2+24))
      test3=unlist(gregexpr('"localites" : ', ExpCarrei)) 
      ExJson=substr(ExpCarrei,test3,LastPoint)
      #PointsE=subset(SiteLoc$nom,grepl(NewCoord$Carre[i],SiteLoc$site))
      #if(nrow(Carrei$localites[[1]])>1){stop("verif recup noms si plusieurs points existants")}
      PointsE=Carrei$localites[[1]]$nom
      if(NewCoord$Point[i] %in% PointsE){stop("nom point doublon malgre distance forte")}
      
      #ExJson=toJSON(Carrei$localites[[1]])
      ExistingLoc=Carrei$localites[[1]]
      ToAdd=ExistingLoc[1,]
      ToAdd$nom=NewCoord$Point[i]
      ToAdd$representatif=F
      ToAdd$geometries$geometries[[1]]$coordinates[[1]][2]=NewCoord$X[i]
      ToAdd$geometries$geometries[[1]]$coordinates[[1]][1]=NewCoord$Y[i]
      ToAddJson=toJSON(ToAdd)
      ToAddJson=gsub("\\]\\]"," ] ",ToAddJson)
      ToAddJson=gsub("\\[\\["," [ ",ToAddJson)
      NewJson=paste0(ExJson,",",ToAddJson)
      NewJson=gsub("\\]\\]"," ] ",NewJson)
      NewJson=gsub("\\[\\["," [ ",NewJson)
      
      #stop("test carre existant")
      sites$update(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   ,paste0('{"$set":{',NewJson,']}}'))
      TimeUpdated=gsub(" ","T",Sys.time())
      TimeUpdated=paste0(TimeUpdated,".000Z")
      sites$update(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   ,paste0('{"$set":{"_updated" : { "$date" : "',TimeUpdated,'" }}}'))
      
      
      
      #sites$update(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
      #            ,paste0('{"$set":{"localites":[{ "nom" : "Z1", "geometries" : { "type" : "GeometryCollection", "geometries" : [ { "type" : "Point", "coordinates" : [ 49.919557076185100186, 3.2183492365574917216 ] } ] }, "representatif" : false }, { "nom" : "Z2", "geometries" : { "type" : "GeometryCollection", "geometries" : [ { "type" : "Point", "coordinates" : [ 49.920703773148623839, 3.2208812418675503153 ] } ] }, "representatif" : false }, { "nom" : "Z3", "geometries" : { "type" : "GeometryCollection", "geometries" : [ { "type" : "Point", "coordinates" : [ 49.921753559244905318, 3.2226691222563275119 ] } ] }, "representatif" : false },',ToAddJson,']}}'))
      
      #,upsert=T)
      
      
      #Carrei$localites[[1]]=ToAdd
      #sites$update('{}',toJSON(Carrei),upsert=T)
      #ToAdd=as.data.frame(ToAdd)
      #row.names(ToAdd)=as.character(max(as.numeric(row.names(NewData$localites[[1]]))+1))
      #row.names(ToAdd$geometries$geometries[[1]])=as.character(max(as.numeric(row.names(NewData$localites[[1]]))+1))
      #Carrei$localites[[1]]=rbindlist(list(Carrei$localites[[1]],ToAdd))
      #stop("coder carre existant")
    }else{
      #NewCoord$Point[i]="Z1"
      #NewSq=c(NewSq,NewCoord$Carre[i])
      #stop("coder création carré")
      NewData=TemplateAddData
      while(NewData$'_etag' %in% alldatasites$'_etag')
      {
        NewEtag=UUIDgenerate()
        NewEtag=gsub("-","",NewEtag)
        NewData$'_etag'=NewEtag
      }
      test=match(NewCoord$Carre[i],CentroidsStoc$numero)
      test2=match(NewCoord$Carre[i],paste0("0",CentroidsStoc$numero))
      if(is.na(test)){test=test2}
      idCarre=CentroidsStoc$id[test]
      NewData$grille_stoc=idCarre
      NewData$'_created'=Sys.time()
      NewData$protocole="54bd090f1d41c8103bad6252" #Point Fixe
      NewData$'_updated'=Sys.time()
      Obsi=users$find(query=paste0('{"email":"',NewCoord$Email[i],'"}'),fields='{}')
      id_observateur= Obsi$'_id'
      NewData$observateur=id_observateur
      NewData$titre=paste0("Vigiechiro - Point Fixe-",NewCoord$Carre[i])
      NewData$verrouille=NA
      class(NewData$localites[[1]])
      NewData$localites[[1]]=NewData$localites[[1]][1,]
      NewData$localites[[1]]$nom=NewCoord$Point[i]
      NewData$localites[[1]]$geometries$geometries[[1]]$coordinates[[1]][1]=NewCoord$Y[i]
      NewData$localites[[1]]$geometries$geometries[[1]]$coordinates[[1]][2]=NewCoord$X[i]
      if(substr(NewCoord$Point[i],1,1)=="Z"){
        NewData$localites[[1]]$representatif=F
      }else{
        NewData$localites[[1]]$representatif=T
      }
      NewData$commentaire=NA
      NewData$generee_aleatoirement=F
      NewData$justification_non_aleatoire=NA
      print(NewData$titre)
      sites$insert(NewData)
      sites$update(paste0('{"titre":"Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   , paste0('{"$set":{"protocole":{ "$oid" : "54bd090f1d41c8103bad6252" }}}')) #protocole
      
      Obsi=users$find(query=paste0('{"email":"',NewCoord$Email[i],'"}'),fields='{}')
      id_observateur= Obsi$'_id'
      
      sites$update(paste0('{"titre":"Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   , paste0('{"$set":{"observateur":{ "$oid" : "',id_observateur,'" }}}')) #observateur
      
      sites$update(paste0('{"titre":"Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   , paste0('{"$set":{"grille_stoc":{ "$oid" : "',idCarre,'" }}}')) #grille_stoc
      SiteLoc=rbind(SiteLoc[1,],SiteLoc)
      SiteLoc$titre[1]=NewData$titre
    }
    
    
    
  }else{
    Carre_existant1=subset(SiteLoc,SiteLoc$titre==paste0("Vigiechiro - Point Fixe-",NewCoord$Carre[i]))
    Carre_existant2=subset(SiteLoc,SiteLoc$titre==paste0("Vigiechiro - Point Fixe-0",NewCoord$Carre[i]))
    if((nrow(Carre_existant1)+nrow(Carre_existant2))>0)
    {
      #stop("test carre existant")
      Carrei=sites$find(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}'),fields='{}') 
      ExpCarrei=capture.output(sites$export(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')))
      
      test=unlist(gregexpr('"representatif" : false', ExpCarrei)) 
      test2=unlist(gregexpr('"representatif" : true', ExpCarrei)) 
      LastPoint=max(c(test+25,test2+24))
      test3=unlist(gregexpr('"localites" : ', ExpCarrei)) 
      ExJson=substr(ExpCarrei,test3,LastPoint)
      #PointsE=subset(SiteLoc$nom,grepl(NewCoord$Carre[i],SiteLoc$site))
      #if(nrow(Carrei$localites[[1]])>1){stop("verif recup noms si plusieurs points existants")}
      PointsE=Carrei$localites[[1]]$nom
      PointsZ=subset(PointsE,substr(PointsE,1,1)=="Z")
      if(length(PointsZ)>0)
      {
        maxE=max(as.numeric(gsub("Z","",PointsZ)))
      }else{
        maxE=0
      }
      NewCoord$Point[i]=paste0("Z",maxE+1)
      if(NewCoord$Point[i] %in% PointsE){stop("nom point doublon malgre distance forte")}
      
      #ExJson=toJSON(Carrei$localites[[1]])
      ExistingLoc=Carrei$localites[[1]]
      ToAdd=ExistingLoc[1,]
      ToAdd$nom=NewCoord$Point[i]
      ToAdd$representatif=F
      ToAdd$geometries$geometries[[1]]$coordinates[[1]][2]=NewCoord$X[i]
      ToAdd$geometries$geometries[[1]]$coordinates[[1]][1]=NewCoord$Y[i]
      ToAddJson=toJSON(ToAdd)
      ToAddJson=gsub("\\]\\]"," ] ",ToAddJson)
      ToAddJson=gsub("\\[\\["," [ ",ToAddJson)
      NewJson=paste0(ExJson,",",ToAddJson)
      #NewJson=gsub("\\]\\]"," ] ",NewJson)
      #NewJson=gsub("\\[\\["," [ ",NewJson)
      
      #stop("test carre existant")
      sites$update(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   ,paste0('{"$set":{',NewJson,']}}'))
      
      TimeUpdated=gsub(" ","T",Sys.time())
      TimeUpdated=paste0(TimeUpdated,".000Z")
      sites$update(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   ,paste0('{"$set":{"_updated" : { "$date" : "',TimeUpdated,'" }}}'))
      
      #row.names(ToAdd)=as.character(max(as.numeric(row.names(NewData$localites[[1]]))+1))
      #row.names(ToAdd$geometries$geometries[[1]])=as.character(max(as.numeric(row.names(NewData$localites[[1]]))+1))
      #NewData$localites[[1]]=rbind(NewData$localites[[1]],ToAdd)
      #stop("coder carre existant")
    }else{
      NewCoord$Point[i]="Z1"
      #NewSq=c(NewSq,NewCoord$Carre[i])
      #stop("coder création carré")
      NewData=TemplateAddData
      while(NewData$'_etag' %in% alldatasites$'_etag')
      {
        NewEtag=UUIDgenerate()
        NewEtag=gsub("-","",NewEtag)
        NewData$'_etag'=NewEtag
      }
      test=match(NewCoord$Carre[i],CentroidsStoc$numero)
      test2=match(NewCoord$Carre[i],paste0("0",CentroidsStoc$numero))
      if(is.na(test)){test=test2}
      idCarre=CentroidsStoc$id[test]
      NewData$grille_stoc=idCarre
      NewData$'_created'=Sys.time()
      NewData$protocole="54bd090f1d41c8103bad6252" #Point Fixe
      NewData$'_updated'=Sys.time()
      Obsi=users$find(query=paste0('{"email":"',NewCoord$Email[i],'"}'),fields='{}')
      id_observateur= Obsi$'_id'
      NewData$observateur=id_observateur
      NewData$titre=paste0("Vigiechiro - Point Fixe-",NewCoord$Carre[i])
      NewData$verrouille=NA
      class(NewData$localites[[1]])
      NewData$localites[[1]]=NewData$localites[[1]][1,]
      NewData$localites[[1]]$nom=NewCoord$Point[i]
      NewData$localites[[1]]$geometries$geometries[[1]]$coordinates[[1]][1]=NewCoord$Y[i]
      NewData$localites[[1]]$geometries$geometries[[1]]$coordinates[[1]][2]=NewCoord$X[i]
      if(substr(NewCoord$Point[i],1,1)=="Z"){
        NewData$localites[[1]]$representatif=F
      }else{
        NewData$localites[[1]]$representatif=T
      }
      NewData$commentaire=NA
      NewData$generee_aleatoirement=F
      NewData$justification_non_aleatoire=NA
      print(NewData$titre)
      sites$insert(NewData)
      sites$update(paste0('{"titre":"Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   , paste0('{"$set":{"protocole":{ "$oid" : "54bd090f1d41c8103bad6252" }}}')) #protocole
      if(!is.na(ParticipanteSpecified)){
        Obsi=users$find(query=paste0('{"email":"',ParticipanteSpecified,'"}'),fields='{}')
        
      }else{
      Obsi=users$find(query=paste0('{"email":"',NewCoord$Email[i],'"}'),fields='{}')
      }
      id_observateur= Obsi$'_id'
      
      sites$update(paste0('{"titre":"Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   , paste0('{"$set":{"observateur":{ "$oid" : "',id_observateur,'" }}}')) #observateur
      
      sites$update(paste0('{"titre":"Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   , paste0('{"$set":{"grille_stoc":{ "$oid" : "',idCarre,'" }}}')) #grille_stoc
      SiteLoc=rbind(SiteLoc[1,],SiteLoc)
      SiteLoc$titre[1]=NewData$titre
      
      #NewJson=toJSON(NewData)
      #NewJson=gsub('"protocole":"54bd090f1d41c8103bad6252"','"protocole":{ "$oid" : "54bd090f1d41c8103bad6252" }',NewJson)
      #Where is ,"observateur":
      #test=unlist(gregexpr(',"observateur":', NewJson))[1]
      #test2=substr(NewJson,1,test+40)
      #test3=substr(NewJson,test+41,nchar(NewJson))
      #NewJson=paste0(test2,"}",test3)
      #NewJson=gsub(',"observateur":',',"observateur":{ "$oid":',NewJson)
      #Where is ,"grille_stoc":
      #test=unlist(gregexpr(',"grille_stoc":', NewJson))[1]
      #test2=substr(NewJson,1,test+40)
      #test3=substr(NewJson,test+41,nchar(NewJson))
      #NewJson=paste0(test2,"}",test3)
      #NewJson=gsub(',"grille_stoc":',',"grille_stoc":{ "$oid":',NewJson)
      #NewJson=toJSON(fromJSON(NewJson))
      #pour réparer les coordonnées
      #NewJson=gsub('\\[\\[','[',NewJson)
      #NewJson=gsub('\\]\\]',']',NewJson)
      #NewJson=toJSON(fromJSON(NewJson))
      #sites$insert(NewJson) 
      #sites$remove('{"titre" : "Vigiechiro - Point Fixe-620341"}')
    }
    
  }
  #stop("recoder cette partie en écrivant dans la base direct")
  
  
  #SiteLoc=rbind(SiteLoc[1,],SiteLoc)
  #SiteLoc$site[1]=NewCoord$Carre[i]
  #SiteLoc$nom[1]=NewCoord$Point[i]
  
}

Existing$Carre=gsub("Vigiechiro - Point Fixe-","",Existing$titre)
Existing$titre=NULL

NewPoints=rbind(Existing,NewCoord)
Tag=Sys.time()
Tag=gsub(" ","_",Tag)
Tag=gsub(":","_",Tag)

NewData=merge(NewPoints,MetadataBMRE,by.x=c("X","Y"),by.y=CoordNames)

fwrite(NewData,paste0("NewPoints_",Tag,".csv"),sep=";")

