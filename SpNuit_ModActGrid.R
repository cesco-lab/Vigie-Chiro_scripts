library(data.table)
library(rgdal)
library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
library(rgeos)
library(latticeExtra)
library(glmmTMB)
#pour afficher les milisecondes
op <- options(digits.secs=3)
Saison=c("05","06","07")


args="DataPF_SpNuit2_Seuil90"
args[2]="gridSIG_Ecart1000_Lat6263000_6363000_Long718000_818000"

#Un premier modèle avec une sélection de variable à la mano  
#liste variables d'interêt (sélection arbitraire)
ListVar=c("n_CLC_23","n_CLC_122","n_CLC_311","n_CLC_112","n_CLC_312","norm_PP","nrm_DtA","n_CLC_242","norm_PC","n_CLC_141","n_CLC_323","norm_Lm","n_CLC_221","n_CLC_41","n_CLC_313","n_CLC_243","n_CLC_211")


#recupération des données chiros
DataCPL3=fread(paste0(args[1],".csv"))

#recup gridSIG
Sys.time()
gridSIG=shapefile(paste0(args[2],".shp"))
Sys.time()
  

#France_departement
FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
Sys.time()


#récupération des données participation
Particip=fread("C:/wamp64/www/p_export.txt")
#récupération des localités
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")

Gite=mapply(function(x,y) 
  ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
                                  ,SiteLoc$commentaire
  ,SiteLoc$localite)
SiteLoc$Gite=as.numeric(Gite)

#formattage de la grille pour la prédiction
gridSIG$Coord=numFactor(gridSIG$crds_x1,gridSIG$crds_x2)
gridSIG$FauxGroupe=rep(1,nrow(gridSIG))



#liste des coordonnées existantes dans ce jeu de données
ListPar=levels(as.factor(DataCPL3$participation))
SelPar=subset(Particip,Particip$participation %in% ListPar)
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
CoordPar=aggregate(SelParSL$participation
                    ,by=c(list(SelParSL$longitude),list(SelParSL$latitude),list(SelParSL$participation))
                    ,FUN=length)

coordinates(CoordPar) <- c("Group.1", "Group.2")
proj4string(CoordPar) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(gridSIG))
CoordL93=spTransform(CoordPar,CRS.new)
CoordSIG=intersect(CoordL93,gridSIG)
PartSIG=as.data.frame(CoordSIG)


gridSIG$Gite=0

ListSp=levels(as.factor(DataCPL3$espece))
#ListSp=c("Barbar","Eptser","Hypsav","Minsch","Myoalc","Myodau","Myoema"
 #        ,"Myomys"
  #       ,"Myonat","Nyclas","Nyclei","Nycnoc","Pipkuh","Pipnat","Pippip"
   #      ,"Pippyg","Pleaus"
    #     ,"Pleaur","Rhifer","Rhihip","Tadten")

FranceWGS84=spTransform(FranceD,CRS(proj4string(CoordPar)))
FranceWGS84 <- SpatialPolygons(list(Polygons(list(Polygon(FranceWGS84)), 1))) 

#FranceWGS84=as(FranceWGS84,"SpatialPolygons")

gridSIG=subset(gridSIG,is.na(gridSIG$CLC_111)==F)


for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i])
  DataSpSL=merge(DataSp,SelParSL,by="participation")
    
    print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  #subset des données correspondant à l'espèce i
  #  if(NbReplicatsSpatiaux>length(ListVar)*30)  
    # {
      
  DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
    DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
    
    DataSaison=merge(DataSpSL_w0,PartSIG
                  ,by.x=c("participation")
                  ,by.y=c("Group.3"))
    
    #seasonal subset
    
    DataSaison=subset(DataSaison,substr(DataSaison$`date part. debut`,4,5) %in% Saison)
    
    
  if(sum(DataSaison$nb_contacts)>0)
  {
    DataSPos=subset(DataSaison,DataSaison$nb_contacts>0)
    NbReplicatsSpatiaux=nlevels(as.factor(as.character(DataSPos$Coord)))
    #Un premier modèle avec une sélection de variable à la mano  
    #liste variables d'interêt (sélection arbitraire)
    if(NbReplicatsSpatiaux>5)
    { 
      
    NbVar=floor(NbReplicatsSpatiaux/10)-1
      
    FormulaTot=paste0("nb_contacts~Gite")
    if(NbVar>0){
    for (h in 1:min(NbVar,length(ListVar)))
    {
      FormulaTot=paste0(FormulaTot,"+",ListVar[h])
    }
    }
    #ajout du terme spatial
    DataSaison$FauxGroupe=rep(1,nrow(DataSaison))
    DataSaison$Coord=numFactor(DataSaison$crds_x1,DataSaison$crds_x2)
    FormulaTot=paste(FormulaTot,"+exp(Coord+0|FauxGroupe)")
    #print(FormulaTot)
    FormulaTot=as.formula(FormulaTot)
    #print(FormulaTot)
    
    
    #test=DataSaison[1:300,]
    Sys.time()
    Mod1=glmmTMB(FormulaTot,data=DataSaison,ziformula=~1,family="nbinom2") # 3 min
    Sys.time()
    save("Mod1",file=paste0(ListSp[i],"_",args[2],"Mod1.RData"))
    Sys.time()
    Pred1=vector()
    for (j in 1:ceiling(nrow(gridSIG)/1000))
    {
      Predtemp=predict(Mod1,newdata=gridSIG[((j-1)*1000+1):(min(j*1000,nrow(gridSIG))),],allow.new.levels=TRUE) # 1000 cell / min
      #Pred1=predict(Mod1,newdata=gridSIG,allow.new.levels=TRUE,se.fit=T) # 1 min
      Pred1=c(Pred1,Predtemp)
      print(paste(j,Sys.time()))
    }
    #polys = as(gridSIG, "SpatialPolygons")
    PredLog=log(Pred1+1,10)
    gridSIG=cbind(gridSIG,PredLog)
    nameGraph=paste0(ListSp[i],"_logActivite_predite_PF")
    names(gridSIG)[ncol(gridSIG)]=nameGraph
    
    MaxScale=quantile(subset(PredLog,PredLog>0.1),0.95)
    if(is.na(MaxScale)){MaxScale=0.1}
    ScaleAt=c(c(0:49)/49*MaxScale,Inf)
    
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                     list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
             
    png(paste0(nameGraph,"_",args[2],"_.png"))
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    dev.off()
    
    win.metafile(paste0(nameGraph,"_",args[2],"_.wmf"))
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    dev.off()
   
    DataPos=subset(DataSaison,DataSaison$nb_contacts>0)
    
    DataPos$MinDec=pmin(DataPos$min_decalage_coucher
                        ,DataPos$min_decalage_lever)
    DataPos=subset(DataPos,DataPos$MinDec>(-1800))
    
    
    if(nrow(DataPos)>10)
    {
    
    
    Sys.time()
    Mod2=glmmTMB(MinDec~1+ exp(Coord + 0 | FauxGroupe),data=DataPos,ziformula=~1,family="gaussian") # 3 min
    Sys.time()
    #save("Mod2",file=paste0(ListSp[i],"_",args[2],"Mod1.RData"))
    #Sys.time()
    Pred2=vector()
    for (j in 1:ceiling(nrow(gridSIG)/1000))
    {
      Predtemp=predict(Mod2,newdata=gridSIG[((j-1)*1000+1):(min(j*1000,nrow(gridSIG))),],allow.new.levels=TRUE) # 1000 cell / min
      #Pred1=predict(Mod1,newdata=gridSIG,allow.new.levels=TRUE,se.fit=T) # 1 min
      Pred2=c(Pred2,Predtemp)
      print(paste(j,Sys.time()))
    }
    #polys = as(gridSIG, "SpatialPolygons")
    gridSIG=cbind(gridSIG,Pred2)
    nameGraph=paste0(ListSp[i],"_DecMin")
    names(gridSIG)[ncol(gridSIG)]=nameGraph
    
    MaxScale=max(Pred2)
    ScaleAt=c(c(0:49)/49*MaxScale,Inf)
    
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    
    
    
    }
    
     
}
  }
    }  

     #spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
     #        list(axis.line = list(col =  'transparent')),col=NA
      #     ,sp.layout=c(list(FranceWGS84),first=T))

fwrite(as.data.table(gridSIG),paste0("gridSIG_",args[2],".csv"))
    

writeOGR(gridSIG,dsn=getwd(),layer=paste0("gridSIG_",args[2]),driver="ESRI Shapefile")
