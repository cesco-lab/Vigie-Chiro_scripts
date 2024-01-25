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


#connection_string = "mongodb://vigiechiro:4Zp9nh8d2YSVt5@ccmg01.in2p3.fr:27030,ccmg02.in2p3.fr:27030,ccmg03.in2p3.fr:27030/vigiechiro?replicaSet=rs0&readPreference=secondaryPreferred&socketTimeoutMS=6000000"
connection_string = "mongodb://vigiechiro:kafc2xdr8vZdBpuV@ccdbmgtstno01.in2p3.fr:27080,ccdbmgtstno03.in2p3.fr:27080,ccdbmgtstno02.in2p3.fr:27080/vigiechiro?replicaSet=rs0&authSource=vigiechiro&socketTimeoutMS=6000000"
MetadataBMRE=fread("C:/Users/yvesb/Downloads/Thomas_Busschaert_Metadata_table.csv")
#id_observateur="5e9886c590250e001113d95d" #VC mnhn
id_observateur="558acd059dcbdc000e0793ee" #Yves Bas
CoordNames=c("X","Y")



sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
grille= mongo(collection="grille_stoc", db="vigiechiro", url=connection_string)

#test=sites$export()


Sys.time()
alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
Sys.time() #~1sec / 1e3 sites

test=sites$find(query = '{"titre" : "Vigiechiro - Point Fixe-340818"}',fields='{}') 



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
CoordOnly=subset(NewCoord,select=CoordNames)

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
coordinates(NewCoordGI)=CoordNames
proj4string(NewCoordGI) <- CRS("+init=epsg:4326")
NewCoordL2E=spTransform(NewCoordGI,CRS("+init=epsg:27572"))

Sys.time()
#SiteLoc=as.data.frame(do.call(rbind,alldatasites$localites))


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



#test 0 : points trop proches dans NewCoord ?
test=gDistance(NewCoordL2E, NewCoordL2E,byid=TRUE)  
testSansDiag=test[-seq(1,(nrow(test)^2),nrow(test)+1)]
Closest=min(testSansDiag)
print(Closest)
if(Closest<30){stop("points doublons dans input table")}


#test 1 : point existant ?
test=gDistance(NewCoordL2E, SiteLocL2E,byid=TRUE)  
Closest=apply(test,2,min)
summary(Closest)


if(min(Closest)<=40)
{
  stop("recoder point d?j? existant")
  Existing=subset(NewCoord,Closest<40)
  dim(Existing)
  Wclosest=apply(test,2,which.min)
  WclosestE=subset(Wclosest,Closest<40)
  Existing$site=SiteLocL2E$site[WclosestE]
  Existing$Point=SiteLocL2E$nom[WclosestE]
  NewCoordL2E=subset(NewCoordL2E,Closest>=40)
  NewCoord=subset(NewCoord,Closest>=40)
}else{
  Existing=NewCoordL2E[0,]
}

test2=gDistance(NewCoordL2E,NewCoordL2E,byid=TRUE)  
test2[test2==0]=999999
ClosestY=apply(test2,2,min)
summary(ClosestY)


if(min(ClosestY)<=40)
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
for (i in 98:nrow(NewCoordL2E))
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
      
      stop("test carre existant")
      sites$update(query = paste0('{"titre" : "Vigiechiro - Point Fixe-',NewCoord$Carre[i],'"}')
                   ,paste0('{"$set":{',NewJson,']}}'))
     
      
      
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
      PointsE=NewData$localites[[1]]$nom
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

