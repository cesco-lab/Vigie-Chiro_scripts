library(data.table)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(readxl)

Table="C:/Users/yvesb/Downloads/BDD_SM2_PourYves.xlsx"


#NewPart=fread("./VigieChiro/PartImport/GMB_2015_2020_MIG14_15_16.csv",dec=",")
#NewPart=fread("./VigieChiro/PartImport/Retransmission_3.csv",dec=",")

SiteLoc=fread("./www/sites_localites.csv")
GridStoc=shapefile("./SIG/carrenat.shp")
outDir="./www/"
id_observateur="55e73d07ee3874000d22d9a4" #Raph Colombo
CoordNames=c("X_WGS84","Y_WGS84")

proj4string(GridStoc) <- CRS("+init=epsg:27572")

if(grepl(".csv",Table))
{
  NewPart=fread(Table)
}else{
  NewPart=read_xlsx(Table)
}



#PF only
SiteLoc=subset(SiteLoc,SiteLoc$protocole=="POINT_FIXE")


#récupérer les centroides
CentroidsStoc = gCentroid(GridStoc,byid=TRUE)
CentroidsWGS84 = spTransform(CentroidsStoc,CRS("+init=epsg:4326"))

#créer les grilles de points représentatifs
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

#récupérer les nouvelles coordonnées
NewCoord=unique(NewPart,by=CoordNames)

NewCoordGI=NewCoord
coordinates(NewCoordGI)=CoordNames
proj4string(NewCoordGI) <- CRS("+init=epsg:4326")
NewCoordL2E=spTransform(NewCoordGI,CRS("+init=epsg:27572"))


SiteLocGI=SiteLoc
coordinates(SiteLocGI)=c("longitude","latitude")
proj4string(SiteLocGI) <- CRS("+init=epsg:4326")
SiteLocL2E=spTransform(SiteLocGI,CRS("+init=epsg:27572"))

#test 1 : point existant ?
test=gDistance(NewCoordL2E, SiteLocL2E,byid=TRUE)  
Closest=apply(test,2,min)
summary(Closest)


if(min(Closest)<=40)
{
  #stop("coder point déjà existant")
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

testNN=gDistance(NewCoordL2E,CentroidsStoc,byid=TRUE) #qq minutes
ClosestN=apply(testNN,2,min)
summary(ClosestN)
NumNN=apply(testNN,2,which.min)

NewCarre=as.character(sprintf("%06d",GridStoc$NUMNAT[NumNN]))
NewCoord$Carre=NewCarre

table(NewCoord$Carre)
test=subset(NewCoord,NewCoord$Carre=="560669")

RefList=list(StocA1,StocA2,StocB1,StocB2,StocC1,StocC2,StocD1,StocD2
             ,StocE1,StocE2,StocF1,StocF2,StocG1,StocG2,StocH1,StocH2)
NameList=c("A1","A2","B1","B2","C1","C2","D1","D2"
           ,"E1","E2","F1","F2","G1","G2","H1","H2")


NewSq=vector()
#tester la grille représentative
for (i in 1:nrow(NewCoordL2E))
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
    NewCoord$Point[i]=NameList[which.min(Dist)]
  }else{
    Carre_existant=max(grepl(NewCoord$Carre[i],SiteLoc$site))
    if(Carre_existant)
    {
      PointsE=subset(SiteLoc$nom,grepl(NewCoord$Carre[i],SiteLoc$site))
      PointsZ=subset(PointsE,substr(PointsE,1,1)=="Z")
      if(length(PointsZ)>0)
      {
        maxE=max(as.numeric(gsub("Z","",PointsZ)))
      }else{
        maxE=0
      }
      NewCoord$Point[i]=paste0("Z",maxE+1)
      #stop("coder carre existant")
    }else{
      NewCoord$Point[i]="Z1"
      NewSq=c(NewSq,NewCoord$Carre[i])
    }
    
  }
  SiteLoc=rbind(SiteLoc[1,],SiteLoc)
  SiteLoc$site[1]=NewCoord$Carre[i]
  SiteLoc$nom[1]=NewCoord$Point[i]
  
}

NewCoord$id_observateur=id_observateur
NewCoord$id_protocole="54bd090f1d41c8103bad6252"
NewCoord$nom=NewCoord$Point
table(NewCoord$nom)
NewCoord$site=paste0("Vigiechiro - Point Fixe-",NewCoord$Carre)

NewCoordC=subset(NewCoord,select=CoordNames)
names(NewCoordC)=c("longitude","latitude")
NewCoord$longitude=NewCoordC$longitude
NewCoord$latitude=NewCoordC$latitude

fwrite(NewCoord,paste0(outDir,"/NewCoord",Sys.Date(),".csv"),sep=";")

PourImportLocalites=subset(NewCoord,select=c("site","id_observateur"
                                             ,"id_protocole","nom","longitude","latitude"))

fwrite(PourImportLocalites,paste0(outDir,"/PourImportLocalites",Sys.Date()
                                  ,".csv"),sep=";")
PourImportSites=subset(NewCoord,NewCoord$Carre %in% NewSq)
test=subset(NewCoord,!NewCoord$Carre %in% NewSq)
print(test$Carre)
PourImportSites=subset(PourImportSites,select=c("Carre","site"
                                                ,"id_observateur"
))
MatchCarre=ifelse(substr(PourImportSites$Carre,1,1)=="0",substr(PourImportSites$Carre,2,6),PourImportSites$Carre)
testIN=match(MatchCarre,GridStoc$NUMNAT)
PourImportSites$longitude_centroid=coordinates(CentroidsWGS84)[testIN,1]
PourImportSites$latitude_centroid=coordinates(CentroidsWGS84)[testIN,2]
PourImportSites=unique(PourImportSites)

fwrite(PourImportSites,paste0(outDir,"/PourImportSites",Sys.Date()
                              ,".csv"),sep=";")

#remettre les références carrés et points dans la table participation
NPcoord=subset(NewPart,select=CoordNames)
names(NPcoord)=c("longitude","latitude")

ExistingC=subset(as.data.frame(Existing),select=CoordNames)
names(ExistingC)=c("longitude","latitude")
Existing$longitude=ExistingC$longitude
Existing$latitude=ExistingC$latitude
Existing$Carre=gsub("Vigiechiro - Point Fixe-","",Existing$site)

AllCoord=rbindlist(list(NewCoord,as.data.frame(Existing)),use.names=T,fill=T)
table(AllCoord$Point)

testcoord=match(paste(NPcoord$longitude,NPcoord$latitude)
                ,paste(AllCoord$longitude,AllCoord$latitude))
if(mean(is.na(testcoord))!=0){stop("missing coordinates")}
NewPart$Carre=AllCoord$site[testcoord]
NewPart$Point=AllCoord$Point[testcoord]

fwrite(NewPart,paste0(outDir,"/NewPart",Sys.Date()
                      ,".csv"),sep=";")


#writeOGR(obj=CentroidSTOC,dsn="./SIG/",layer="carrenat_centroids.shp",driver="ESRI Shapefile")
