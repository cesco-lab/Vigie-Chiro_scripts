library(data.table)
library(rgdal)
library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
library(rgeos)
library(latticeExtra)
library(randomForest)
#library(glmmTMB)
library(gdata)
library(spdep)
#pour afficher les milisecondes
op <- options(digits.secs=3)
#Saison=c("05","06","07") #obsolete


#args="ETV_filtree"
args="./VigieChiro/GIS/PA/PA_Dama dama"
args="./VigieChiro/GIS/PA/PATot_FR4___130"
#args="./VigieChiro/GIS/Capture/PA_PKPN_Pipistrellus kuhlii"
#args="./VigieChiro/GIS/Capture/PA_Plecos_Plecotus austriacus"
#args="./VigieChiro/GIS/Capture/Myotis"
#args[2]="GI_coordWGS84_SpNuit2_Seuil50_DataLP_PF_exportTot_Lat41.45_51.61_Long-5.9_9.73"
args[2]="PA/GI_PATot_FR4___130" #coordSIG table
#args[2]="Capture/GI_site_capture_chiro"
args[3]="SpeciesList.csv"
args[4]="Esp" #name of taxa column
args[5]="bat" #name of taxa group (useless if args[3] is specified)
DataLoc=F #if data contains a locality field
#args[6]="pk_carre.x" #name of locality in CoordSIG
args[6]="Group.3" #name of sampling event
args[6]=NA
args[7]="carre" #name of locality in Data
#args[8]="id_carre_annee" #name of participation (=sampling event)
args[8]=NA #name of participation (=sampling event)
args[9]=F #if date (=day-of-year) is provided or not
args[10]="presence"
DataCoord=T #if data contains exact coordinates (=same as CoordSIG table)
#Coord=c("Group.1", "Group.2")
Coord=c("decimalLongitude", "decimalLatitude")
#Coord=c("X_CENTROID", "Y_CENTROID")
args[11]=40 #ncoord = number of coordinates projections (must be a division of 360)


#recupération des données chiros
if(dir.exists(args[1]))
{
  ListData=list.files(args[1],full.names=T)
}else{
  ListData=paste0(args[1],".csv")
}

for (h in 1:length(ListData))
{
  DataCPL3=fread(ListData[h])
  
  #recup gridSIG
  Sys.time()
  CoordSIG=fread(paste0("./VigieChiro/GIS/",args[2],".csv"))
  Sys.time()
  if(sum(grepl("Group.1",names(CoordSIG)))>0)
  {
    CoordSIG$Group.1=CoordSIG$Group.1.x
    CoordSIG$Group.2=CoordSIG$Group.2.x
    CoordSIG$Group.1.x=NULL
    CoordSIG$Group.1.y=NULL
    CoordSIG$Group.2.x=NULL
    CoordSIG$Group.2.y=NULL
  }
  
  SpeciesList=fread(args[3])
  
  
  
  
  #France_departement
  #FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
  #Sys.time()
  
  if(DataCoord)
  {
    SelParSL=subset(DataCPL3,select=Coord)
    SelParSL=unique(SelParSL)
    CoordPar0=subset(CoordSIG,select=Coord)
    CoordPar=merge(CoordPar0,SelParSL,by=Coord)
    if(ncol(CoordPar)>3){
      names(CoordPar)[4]="Group.3"
    }
    CoordPS=merge(SelParSL,CoordSIG,by=Coord)
    
  }else{
    
    
    if(!DataLoc)
    {
      
      #récupération des données participation
      Particip=fread("C:/wamp64/www/p_export.csv")
      #récupération des localités
      SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
      Gite=mapply(function(x,y) 
        ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
        ,SiteLoc$commentaire
        ,SiteLoc$localite)
      SiteLoc$SpGite=as.numeric(Gite)
      
      #liste des coordonnées existantes dans ce jeu de données
      ListPar=levels(as.factor(DataCPL3$participation))
      SelPar=subset(Particip,Particip$participation %in% ListPar)
      SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
      CoordPar=aggregate(SelParSL$participation
                         ,by=c(list(SelParSL$longitude),list(SelParSL$latitude),list(SelParSL$participation))
                         ,FUN=length)
      CoordPar$x=NULL
      
    }else{
      ColCode1=match(args[6],names(CoordSIG))
      ColCode2=match(args[7],names(DataCPL3))
      ColCode3=match(args[8],names(DataCPL3))
      DataCPL3$participation=as.data.frame(DataCPL3)[,ColCode3]
      DataCPL3$localite=as.data.frame(DataCPL3)[,ColCode2]
      SelParSL=subset(DataCPL3,select=c("participation","localite"))
      SelParSL=unique(SelParSL)
      CoordPar0=subset(CoordSIG,select=c("Group.1","Group.2",args[6]))
      CoordPar=merge(CoordPar0,SelParSL,by.x=args[6],by.y="localite")
      names(CoordPar)[4]="Group.3"
      #SelParSL=CoordSIG
      #SelParSL$participation=CoordPar$Group.3
      CoordPS=CoordPar
      #ColLat=match(args[7],names(DataCPL3))  
      #Latitude=as.data.frame(DataCPL3)[,ColLat]  
      #Latitude=as.numeric(gsub(",",".",Latitude))
    }
  }
  
  CoordPS$individualCount=NULL
  CoordPS$individualCount.y=NULL
  CoordPS$individualCount.x=NULL
  CoordPS$presence=NULL
  CoordPS$coordinateUncertaintyInMeters=NULL
  CoordPS$canonicalName=NULL
  
  
  test=(is.na(CoordPS))
  test2=apply(test,MARGIN=1,sum)
  test3=apply(test,MARGIN=2,sum)
  test3
  CoordPS=subset(CoordPS,test2==0)
  
  if(is.na(args[6]))
  {
    CoordPS$participation=c(1:nrow(CoordPS))
  }else{
    testPar=grepl(args[6],names(CoordPS))
    numPar=subset(c(1:length(testPar)),testPar)
    CoordPS$participation=as.data.frame(CoordPS)[,numPar[1]]
  }
  
  
  if(!is.na(args[3]))
  {
    SpeciesList=fread(args[3])
    if(sum(grepl("espece",names(DataCPL3)))==0)
    {
      DataCPL3$valid.espece=substr(args[1],nchar(args[1])-5,nchar(args[1]))
    }
    ListSp=levels(as.factor(DataCPL3$valid.espece))
    
    
  }else{
    Group=args[5]
    colTaxa=match(args[4],names(DataCPL3))
    DataCPL3$espece=as.data.frame(DataCPL3)[,colTaxa]
    Esp=unique(as.data.frame(DataCPL3)[,colTaxa])
    ListSp=levels(as.factor(Esp))
    Metric=args[10]
    DataCPL3$nb_contacts=subset(DataCPL3,select=Metric)
    SpeciesList=data.table(cbind(Group,Esp))
    fwrite(SpeciesList,paste0("SpeciesList_",Group,substr(Sys.time(),1,10),".csv"))
  }
  
  #ListSp=levels(as.factor(DataCPL3$valid.espece))
  #ListSp=c("Barbar","Eptser","Hypsav","Minsch","Myoalc","Myodau","Myoema"
  #        ,"Myomys"
  #       ,"Myonat","Nyclas","Nyclei","Nycnoc","Pipkuh","Pipnat","Pippip"
  #      ,"Pippyg","Pleaus"
  #     ,"Pleaur","Rhifer","Rhihip","Tadten")
  
  if(!(DataCPL3$valid.espece[1] %in% ListSp))
  {
    ListSp=levels(as.factor(DataCPL3$valid.espece))
  }
  
  
  for (i in 1:length(ListSp))
  {
    DataSp=subset(DataCPL3,DataCPL3$valid.espece==ListSp[i])
    DataSp$presence[is.na(DataSp$presence)]=0
    if(sum(grepl("participation",names(DataSp)))>0)
    {
      DataSp=unique(DataSp,by="participation")
      DataSp$presence=1
      DataSpSL=merge(DataSp,SelParSL,by="participation")
      
      print(paste(ListSp[i],nrow(DataSp),Sys.time()))
      #subset des données correspondant à l'espèce i
      #  if(NbReplicatsSpatiaux>length(ListVar)*30)  
      # {
      
      DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
      DataSpSL_w0$presence[is.na(DataSpSL_w0$presence)]=0
      DataSaison=merge(DataSpSL_w0,CoordPS
                       ,by.x=c("participation")
                       ,by.y=c("Group.3"))
      
    }else{
      if(sum(grepl("presence",names(DataSp)))==0)
      {
        
        DataSp$presence=as.numeric(DataSp$Esp==DataSp$valid.espece)
        
      }
      DataSaison=merge(DataSp,CoordPS
                       ,by=Coord)
      
    }
    
    
    
    
    
    
    
    #add date of year
    if(args[9])
    {
      if(sum(grepl("date_debut",names(DataSaison)))>0)
      {
        Date1=as.Date(substr(DataSaison$date_debut,1,10)
                      ,format="%d/%m/%Y")      
      }else{
        Date1=as.Date(substr(DataSaison$DATE_POSIX,1,10)
                      ,format="%Y-%m-%d")      
      }
      
      DataSaison$SpFDate=yday(Date1)
    }else{
      DataSaison$SpFDate=0
      
    }
    #add several rotated coordinates
    testCoord=match(Coord,names(DataSaison))
    CoordDS=as.matrix(cbind(as.data.frame(DataSaison)[,testCoord[1]]
                            ,as.data.frame(DataSaison)[,testCoord[2]]))
    
    for (a in 0:(as.numeric(args[11])-1))
    {
      Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))
      #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
      #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
      DataSaison=cbind(DataSaison,Coordi[,1])
      names(DataSaison)[ncol(DataSaison)]=paste0("SpCoord",a)
    }
    
    #seasonal subset
    #  DataSaison=subset(DataSaison,substr(DataSaison$`date part. debut`,4,5) %in% Saison)
    
    
    if(sum(DataSaison$presence)>0)
    {
      DataSPos=subset(DataSaison,DataSaison$presence>0)
      #NbReplicatsSpatiaux=nlevels(as.factor(as.character(DataSPos$Coord)))
      
      testPred=(substr(names(DataSaison),1,2)=="Sp")
      Prednames=names(DataSaison)[testPred]
      Predictors=DataSaison[,..Prednames]
      
      
      testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
      
      print(nrow(DataSaison))
      
      #Predictors$SpAltiS[is.na(Predictors$SpAltiS)]=0
      
      testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
      
      Sys.time()
      ModRF=randomForest(x=Predictors,y=as.factor(DataSaison$presence)
                         ,replace=T
                         ,strata=paste(DataSaison$id_site,DataSaison$localite)
                         ,importance=T) #0.1 sec / tree
      Sys.time()
      
      varImpPlot(ModRF,cex=0.5,main=paste("Presence",ListSp[i]))
      print(paste("TxErreur: ",ModRF$confusion[2,3]))
      #coordinates(DataSaison) <- c("Group.1", "Group.2")
      #proj4string(DataSaison) <- CRS("+init=epsg:4326") # WGS 84
      #DataSaison$pred=ModRF$predicted
      #print(spplot(DataSaison,zcol="pred",main=ListSp[i]))  
      
      
      save (ModRF,file=paste0("./VigieChiro/ModPred/ModRFPresence_",ListSp[i]
                              ,substr(ListData[h],nchar(ListData[h])-11
                                      ,nchar(ListData[h])-4)
                              ,".learner")) 
      
    }
  }
  
}
