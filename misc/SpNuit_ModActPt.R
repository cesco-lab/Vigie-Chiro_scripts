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


args="GpNuit2_50_DataLP_PF_exportTot"
#args="./VigieChiro/STOC-EPS/data_FrenchBBS_squarre_Diane_20180628_allSp_2001_2018"

args[2]="GI_coordWGS84_SpNuit2_50_DataLP_PF_exportTot"
#args[2]="GI_coordWGS84_ALL"
args[3]="SpeciesList.csv"
args[3]=NA
args[4]="Esp" #name of taxa column (useless if args[3] is specified)
args[4]="espece" #name of taxa column (useless if args[3] is specified)
#args[4]="code_sp" #name of taxa column (useless if args[3] is specified)
args[5]="bat" #name of taxa group (useless if args[3] is specified)
DataLoc=F
args[6]="Group.3" #name of sampling event
args[7]="carre" #name of locality in CoordSIG (if DataLoc)
args[8]="id_carre_annee" #name of participation (=sampling event)
args[9]=T #if date (=day-of-year) is provided or not
#args[10]="abondance"
args[10]="nb_contacts"
#args[6]="longitude_grid_wgs84"
#args[7]="latitude_grid_wgs84"
args[11]=40 #number of coordinates projections (must be a division of 360)
MinData=1
GroupSel="bat"
DM=F

#recupération des données chiros
DataCPL3=fread(paste0(args[1],".csv"))

#recup gridSIG
Sys.time()
CoordSIG=fread(paste0("./VigieChiro/GIS/",args[2],".csv"))
Sys.time()
if(!match("Group.1",names(CoordSIG)))
{
CoordSIG$Group.1=CoordSIG$Group.1.x
CoordSIG$Group.2=CoordSIG$Group.2.x
CoordSIG$Group.1.x=NULL
CoordSIG$Group.1.y=NULL
CoordSIG$Group.2.x=NULL
CoordSIG$Group.2.y=NULL
}



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

#ColLat=match(args[7],names(DataCPL3))  
#Latitude=as.data.frame(DataCPL3)[,ColLat]  
#Latitude=as.numeric(gsub(",",".",Latitude))
}



CoordPS=merge(CoordPar,CoordSIG,by=c("Group.1","Group.2"))

test=(is.na(CoordPS))
test2=apply(test,MARGIN=1,sum)
test3=apply(test,MARGIN=2,sum)
plot(test2)
plot(test3)

CoordPS=subset(CoordPS,test2==0)
testPar=grepl(args[6],names(CoordPS))
numPar=subset(c(1:length(testPar)),testPar)
CoordPS$participation=as.data.frame(CoordPS)[,numPar[1]]

if(!is.na(args[3]))
   {
SpeciesList=fread(args[3])
ListSp=levels(as.factor(DataCPL3$espece))

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



#France_departement
#FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
#Sys.time()



if(!is.na(GroupSel))
{
  SpSel=subset(SpeciesList,SpeciesList$Group %in% GroupSel)
  ListSp=subset(ListSp,ListSp %in% SpSel$Esp)
}

#ListSp=c("Barbar","Eptser","Hypsav","Minsch","Myoalc","Myodau","Myoema"
#        ,"Myomys"
#       ,"Myonat","Nyclas","Nyclei","Nycnoc","Pipkuh","Pipnat","Pippip"
#      ,"Pippyg","Pleaus"
#     ,"Pleaur","Rhifer","Rhihip","Tadten")



for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i])
  
  DataSpSL=merge(DataSp,SelParSL,by="participation")
  fwrite(DataSpSL,paste0("./VigieChiro/DataSp/DataSpSL_",ListSp[i],".csv"))
  
  print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  #subset des données correspondant à l'espèce i
  #  if(NbReplicatsSpatiaux>length(ListVar)*30)  
  # {
  
  DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
  DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0

  DataSaison=merge(DataSpSL_w0,CoordPS
                   ,by.x=c("participation")
                   ,by.y=c("Group.3"))
  
  
  
  
  #add date of year
  if(args[9])
  {
  Date1=as.Date(substr(DataSaison$date_debut,1,10)
                ,format="%d/%m/%Y")
  DataSaison$SpFDate=yday(Date1)
  }else{
    DataSaison$SpFDate=0
    
  }
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
  
  
  if(sum(DataSaison$nb_contacts)>=MinData)
  {
    DataSPos=subset(DataSaison,DataSaison$nb_contacts>0)
    #NbReplicatsSpatiaux=nlevels(as.factor(as.character(DataSPos$Coord)))
    
    testPred=(substr(names(DataSaison),1,2)=="Sp")
    Prednames=names(DataSaison)[testPred]
    Predictors=DataSaison[,..Prednames]
    
    
    testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
    
    DataSaison$ActLog10=log10(DataSaison$nb_contacts+1)
    
    Sys.time()
    ModRF=randomForest(x=Predictors,y=DataSaison$ActLog10
                                ,replace=T
                                ,strata=paste(DataSaison$id_site,DataSaison$localite)
                                ,importance=T
                       ,mtry=5) #2.1 sec / tree
    Sys.time()
    
    varImpPlot(ModRF,cex=0.5,main=paste("Act",ListSp[i]))
    print(paste("PseudoR2: ",ModRF$rsq[ModRF$ntree]))
    #coordinates(DataSaison) <- c("Group.1", "Group.2")
    #proj4string(DataSaison) <- CRS("+init=epsg:4326") # WGS 84
    #DataSaison$pred=ModRF$predicted
    #print(spplot(DataSaison,zcol="pred",main=ListSp[i]))  
    
    #test if species is a bat
    test=match(ListSp[i],SpeciesList$Esp)
    Bat=(SpeciesList$Group[test]=="bat")
    
    save (ModRF,file=paste0("./VigieChiro/ModPred/ModRFActLog_",ListSp[i]
                            ,substr(args[1],nchar(args[1])-7,nchar(args[1]))
                            ,".learner")) 
    
    
    if((Bat)&(DM))
    {
    DataSaison$DM=pmin(DataSaison$min_decalage_coucher,DataSaison$min_decalage_lever)
    DataSaisonDM=subset(DataSaison,DM>0)
    
    testPredDM=(substr(names(DataSaisonDM),1,7)=="SpCoord")
    Prednames=names(DataSaisonDM)[testPredDM]
    Prednames=c(Prednames,"SpFDate")
    PredictorsDM=as.data.table(DataSaisonDM)[,..Prednames]
    
    DataSaisonDM$Log10DM=log10(DataSaisonDM$DM/60+1)
    
    Sys.time()
    ModRF_DM=randomForest(x=PredictorsDM,y=DataSaisonDM$Log10DM
                       ,replace=T
                       ,strata=paste(DataSaisonDM$id_site,DataSaisonDM$localite)
                       ,importance=T) #0.1 sec / tree
    Sys.time()
    #varImpPlot(ModRF_DM,cex=0.5,main=paste("DM",ListSp[i]))
    
    save (ModRF_DM,file=paste0("./VigieChiro/ModPred/ModRFDecMin_",ListSp[i]
                               ,substr(args[1],nchar(args[1])-7,nchar(args[1]))
                               ,".learner")) 
    
    #DataSaisonDM$predDM=ModRF_DM$predicted
    #print(spplot(DataSaisonDM,zcol="predDM",main=paste(ListSp[i],"DM")))  
    
    }
  }
}

