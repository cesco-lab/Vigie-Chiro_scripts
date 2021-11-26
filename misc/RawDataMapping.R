library(data.table)
#library(rgdal)
library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
#library(rgeos)
#library(latticeExtra)
#library(randomForest)
#library(gdata)
#library(spdep)
#pour afficher les milisecondes
op <- options(digits.secs=3)
#Saison=c("05","06","07") #obsolete


args="C:/wamp64/www/SpNuit2_90_DataLP_PF_exportTot"
#args="./VigieChiro/STOC-EPS/data_FrenchBBS_squarre_Diane_20180628_allSp_2001_2018"

args[3]="SpeciesList.csv"
#args[3]=NA
args[4]="Esp" #name of taxa column (useless if args[3] is specified)
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
#FLimite="France_dep_L93_09_11_12_30_31_32_34_46_48_65_66_81_82_2A_2B_13_83_84_04_05_06_07_26_69_42_38_73_74_01_03_63_43_15_19_23_87_86_79_17_16_33_40_47_64_24.shp"
FLimite="France_dep_L93.shp"
#FLimite="SMPNR_AdminExpress.shp"
w0=T


Limite=shapefile(paste0("./VigieChiro/GIS/",FLimite))
Sys.time()

LimiteL=as(Limite,'SpatialLines')


#recupération des données chiros
DataCPL3=fread(paste0(args[1],".csv"))

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




#ListSp=c("Barbar","Eptser","Hypsav","Minsch","Myoalc","Myodau","Myoema"
#        ,"Myomys"
#       ,"Myonat","Nyclas","Nyclei","Nycnoc","Pipkuh","Pipnat","Pippip"
#      ,"Pippyg","Pleaus"
#     ,"Pleaur","Rhifer","Rhihip","Tadten")



for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i])
  if(nrow(DataSp)>1)
  {
    DataSpSL=merge(DataSp,SelParSL,by="participation")
    if(!w0)
    {
    fwrite(DataSpSL,paste0("./VigieChiro/DataSp/DataSpSL_",ListSp[i],"_"
                           ,substr(basename(args[1]),9,15),".csv"))
    }
    print(paste(ListSp[i],nrow(DataSp),Sys.time()))
    #subset des données correspondant à l'espèce i
    #  if(NbReplicatsSpatiaux>length(ListVar)*30)  
    # {
    
    DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
    DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
    
    if(w0)
    {
      fwrite(DataSpSL_w0,paste0("./VigieChiro/DataSp/DataSpSLw0_",ListSp[i],"_"
                             ,substr(basename(args[1]),9,15),".csv"))
    }
    
    
    coordinates(DataSpSL)=c("longitude","latitude")
    proj4string(DataSpSL) <- CRS("+init=epsg:4326") # WGS 84
    
    CRSL93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")
    DataSpL93=spTransform(DataSpSL,CRSL93)
    
    Taxon=ListSp[i]
    test=match(Taxon,SpeciesList$Esp)
    if(is.na(test))
    {
      Title=Taxon
    }else{
      Title=SpeciesList$NomFR[test]
    }
    
    Title=paste(Title,"(données brutes)")  
    Size=(DataSpL93$nb_contacts/max(DataSpL93$nb_contacts))^(0.5)
    #plotting abundances
    GraphName=paste0("./VigieChiro/Maps/Raw/",ListSp[i],"_"
                     ,substr(basename(args[1]),9,15),"_Raw.png")
    png(GraphName,width = 600, height = 600)
    
    p=spplot(DataSpL93,zcol="nb_contacts",sp.layout = LimiteL
             ,xlim = bbexpand(bbox(LimiteL)[1,],0.04)
             ,ylim = bbexpand(bbox(LimiteL)[2,],0.04),col="transparent"
             ,par.settings =
               list(axis.line = list(col =  'transparent'))
             ,cex=Size*3,colorkey=F,col.regions=4
             ,key=list(lines=TRUE, col="transparent") #this last line to remove the legend
             ,pch=1,main=Title) 
    
    print(p)
    dev.off()
  }
}

