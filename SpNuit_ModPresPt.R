library(data.table)
library(rgdal)
library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
library(rgeos)
library(latticeExtra)
library(randomForest)
library(glmmTMB)
library(gdata)
library(spdep)
#pour afficher les milisecondes
op <- options(digits.secs=3)
#Saison=c("05","06","07") #obsolete


args="ETV_filtree"
args[2]="GI_coordWGS84_SpNuit2_Seuil50_DataLP_PF_exportTot_Lat41.45_51.61_Long-5.9_9.73"
args[3]="SpeciesList.csv"
args[11]=40 #ncoord = number of coordinates projections (must be a division of 360)


#recupération des données chiros
DataCPL3=fread(paste0(args[1],".csv"))

#recup gridSIG
Sys.time()
CoordSIG=fread(paste0("./VigieChiro/GIS/",args[2],".csv"))
Sys.time()
CoordSIG$Group.1=CoordSIG$Group.1.x
CoordSIG$Group.2=CoordSIG$Group.2.x
CoordSIG$Group.1.x=NULL
CoordSIG$Group.1.y=NULL
CoordSIG$Group.2.x=NULL
CoordSIG$Group.2.y=NULL

SpeciesList=fread(args[3])




#France_departement
#FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
#Sys.time()

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

CoordPS=merge(CoordPar,CoordSIG,by=c("Group.1","Group.2"))

test=(is.na(CoordPS))
test2=apply(test,MARGIN=1,sum)
CoordPS=subset(CoordPS,test2==0)


ListSp=levels(as.factor(DataCPL3$valid.espece))
#ListSp=c("Barbar","Eptser","Hypsav","Minsch","Myoalc","Myodau","Myoema"
#        ,"Myomys"
#       ,"Myonat","Nyclas","Nyclei","Nycnoc","Pipkuh","Pipnat","Pippip"
#      ,"Pippyg","Pleaus"
#     ,"Pleaur","Rhifer","Rhihip","Tadten")



for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$valid.espece==ListSp[i])
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
  
  
  
  
  #add date of year
  
  Date1=as.Date(substr(DataSaison$date_debut,1,10)
                ,format="%d/%m/%Y")
  DataSaison$SpFDate=yday(Date1)
  
  #add several rotated coordinates
  CoordDS=as.matrix(cbind(DataSaison$Group.1,DataSaison$Group.2))
  
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
                            ,substr(args[1],nchar(args[1])-7,nchar(args[1]))
                            ,".learner")) 
    
      }
}

