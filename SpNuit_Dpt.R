library(data.table)
#library(rgdal)
library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
library(rgeos)
library(latticeExtra)
#pour afficher les milisecondes
op <- options(digits.secs=3)

args="DataPF_SpNuit2_Seuil90"
#recupération des données chiros
DataCPL3=fread(paste0(args,".csv"))

#France_departement
FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
Sys.time()


#récupération des données participation
Particip=fread("C:/wamp64/www/p_export.txt")
#récupération des localités
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")

#liste des coordonnées existantes dans ce jeu de données
ListPar=levels(as.factor(DataCPL3$participation))
SelPar=subset(Particip,Particip$participation %in% ListPar)
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
CoordPar=aggregate(SelParSL$participation
                    ,by=c(list(SelParSL$longitude),list(SelParSL$latitude),list(SelParSL$participation))
                    ,FUN=length)

coordinates(CoordPar) <- c("Group.1", "Group.2")
proj4string(CoordPar) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(FranceD))
CoordL93=spTransform(CoordPar,CRS.new)
CoordFranceD=intersect(CoordL93,FranceD)
PartFranceD=as.data.frame(CoordFranceD)

ListSp=levels(as.factor(DataCPL3$espece))


for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i])
  DataSpSL=merge(DataSp,SelParSL,by="participation")
  NbReplicatsSpatiaux=nlevels(as.factor(paste(DataSpSL$longitude,DataSpSL$latitude)))
  print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  #subset des données correspondant à l'espèce i
    DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
    DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
    
    DataSIG=merge(DataSpSL_w0,PartFranceD
                  ,by.x=c("participation")
                  ,by.y=c("Group.3"))
    
  AggDep=aggregate(DataSIG$nb_contacts,by=list(DataSIG$DépARTEM0),FUN=mean)
  RepDep=aggregate(DataSIG$nb_contacts,by=list(DataSIG$DépARTEM0),FUN=length)
  #hist(log(AggDep$x+1),breaks=50)
  AggDep$Act=log(AggDep$x+1)
  #AggDep$Act=AggDep$x
  #AggDep$Act_scale=AggDep$Act/mean(AggDep$Act)
  AggDep$Act_scale=AggDep$Act/mean(AggDep$Act)
  #AggDep$Act_scale=AggDep$Act_scale/sd(AggDep$Act_scale)
  ARDep=merge(AggDep,RepDep,by="Group.1")
  
  
    CarteActDep=merge(FranceD,AggDep,by.x="DépARTEM0",by.y="Group.1")
    CarteActDep2=merge(CarteActDep,RepDep,by.x="DépARTEM0",by.y="Group.1")
    breaks <- c(-Inf,-998,seq(0.1, 2, by=0.1),Inf)
    cols <- c("grey",colorRampPalette(c("white", "red"))(length(breaks)-1))
    CarteActDep2$Act_map=CarteActDep2$Act_scale
    CarteActDep2$Act_map[is.na(CarteActDep2$Act_map)]=(-999)
    CarteActDep2$Act_map[(CarteActDep2$x.y<5)]=(-999)
    
    CarteActDep2$IntAct=findInterval(CarteActDep2$Act_map,vec=breaks)
    CarteActDep2$colAct=cols[CarteActDep2$IntAct]
  win.metafile(file=paste0(ListSp[i],"_",args,"Dep_Act.wmf"))
  print(  spplot(CarteActDep2,zcol="IntAct",col.regions=cols,colorkey=F,par.settings =
             list(axis.line = list(col =  'transparent')),main=paste(ListSp[i],"/ Point Fixe - Activité"))
)
           #Sys.sleep(0.5)
  dev.off() 
fwrite(ARDep,paste0(ListSp[i],"_",args,"ARDep.csv"))
 
print(paste(ListSp[i],NbReplicatsSpatiaux))
   }

 