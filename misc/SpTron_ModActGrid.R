library(data.table)
#library(rgdal)
library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
library(rgeos)
library(latticeExtra)
library(glmmTMB)
#pour afficher les milisecondes
op <- options(digits.secs=3)

args="DataRP_SpTronSeuil90"
TriDirectUniquement=T

#Un premier modèle avec une sélection de variable à la mano  
#liste variables d'interêt (sélection arbitraire)
ListVar=c("n_CLC_23","n_CLC_122","n_CLC_311","n_CLC_112","n_CLC_312","norm_PP","nrm_DtA","n_CLC_242","norm_PC","n_CLC_141","n_CLC_323","norm_Lm","n_CLC_221","n_CLC_411","n_CLC_313","n_CLC_243","n_CLC_211","n_CLC_241")


#recupération des données chiros
DataCPL3=fread(paste0(args,".csv"))

#recup gridSIG
Sys.time()
gridSIG=shapefile("gridSIG.shp")
Sys.time()
  

#France_departement
FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
Sys.time()


#récupération des données participation
Particip=fread("C:/wamp64/www/p_export.txt",encoding="UTF-8")
#récupération des localités
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")


#formattage de la grille pour la prédiction
gridSIG$Coord=numFactor(gridSIG$crds_x1,gridSIG$crds_x2)
gridSIG$FauxGroupe=rep(1,nrow(gridSIG))

SpTron=DataCPL3
SpTron=subset(SpTron,(SpTron$temps_enr<600)&
                (SpTron$temps_enr>200))

SpTronP=merge(SpTron,Particip,by="participation")

SpTronP$Expansion=(((SpTronP$num_micro)
                    &(SpTronP$canal_expansion_temps=="DROITE"))
                   |((SpTronP$num_micro==F)
                     &(SpTronP$canal_expansion_temps=="GAUCHE")))
SpTronP$Direct=(((SpTronP$num_micro)
                 &(SpTronP$canal_enregistrement_direct=="DROITE"))
                |((SpTronP$num_micro==F)
                  &(SpTronP$canal_enregistrement_direct=="GAUCHE")))

#CanalBug=subset(SpTronP,(Expansion&Direct))
test=subset(SpTron,SpTron$espece=="Nycnoc")
plot(test$temps_enr,test$nb_contacts,log="y")

Prot=substr(SpTronP$site,1,21)
SpTronP$Prot=Prot
SpTronRE=subset(SpTronP,(Prot=="Vigie-chiro - Routier")&(Expansion))
SpTronRD=subset(SpTronP,(Prot=="Vigie-chiro - Routier")&(Direct))
SpTronPE=subset(SpTronP,(Prot=="Vigiechiro - Pédestre")&(Expansion))
SpTronPD=subset(SpTronP,(Prot=="Vigiechiro - Pédestre")&(Direct))

if(TriDirectUniquement)
{
  SpTronP=subset(SpTronP,SpTronP$Direct)
}
SpTronP$Tron=as.factor(SpTronP$Tron)


ListSp=levels(as.factor(DataCPL3$espece))



#SiteLoc
#PourTron=tstrsplit(SiteLoc$nom," ")
SiteLoc$Tron=sapply(SiteLoc$nom
                    ,FUN=function(x) if(substr(x,1,1)=="T"){tstrsplit(x," ")[[2]]}else{x})

SiteLocU=unique(SiteLoc,by=c("site","Tron"))
SiteLocU=subset(SiteLocU,SiteLocU$protocole!="POINT_FIXE")

PSL=merge(Particip,SiteLocU,by="site",allow.cartesian=TRUE)
PSL=subset(PSL,PSL$protocole!="POINT_FIXE")
PSL_R=subset(PSL,PSL$protocole=="ROUTIER")

CoordPar=aggregate(PSL_R$participation
                   ,by=c(list(PSL_R$longitude),list(PSL_R$latitude),list(PSL_R$participation),list(PSL_R$Tron))
                   ,FUN=length)


FranceWGS84=spTransform(FranceD,CRS(proj4string(CoordPar)))
#FranceWGS84 <- SpatialPolygons(list(Polygons(list(Polygon(FranceWGS84)), 1))) 
testP=match(SpTronP$participation,CoordPar$Group.3)
SpTronP_P=subset(SpTronP,is.na(testP))
SpTronP_R=subset(SpTronP,is.na(testP)==F)




#liste des coordonnées existantes dans ce jeu de données
coordinates(CoordPar) <- c("Group.1", "Group.2")
proj4string(CoordPar) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(gridSIG))
CoordL93=spTransform(CoordPar,CRS.new)
CoordSIG=intersect(CoordL93,gridSIG)
PartSIG=as.data.frame(CoordSIG)
#spplot(CoordSIG,zcol="norm_Lm")



for (i in 1:length(ListSp))
{
  DataSp=subset(SpTronP_R,SpTronP_R$espece==ListSp[i])
  DataSpSL=merge(DataSp,CoordPar,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"))
  NbReplicatsSpatiaux=nlevels(as.factor(paste(DataSpSL$Group.1,DataSpSL$Group.2)))
  print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  #subset des données correspondant à l'espèce i
  DataSpSL_w0=merge(DataSp,PartSIG,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"),all.y=T)
  DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
  
    DataSIG=DataSpSL_w0
    
  if(sum(DataSIG$nb_contacts)>0)
  {
    DataSPos=subset(DataSIG,DataSIG$nb_contacts>0)
    NbReplicatsSpatiaux=nlevels(as.factor(as.character(DataSPos$Coord)))
    #Un premier modèle avec une sélection de variable à la mano  
    #liste variables d'interêt (sélection arbitraire)
    if(NbReplicatsSpatiaux>10)
    { 
      
    NbVar=floor(NbReplicatsSpatiaux/10)  
      
    FormulaTot=paste0("nb_contacts~1")
    if(NbVar>0){
    for (h in 1:min(NbVar,length(ListVar)))
    {
      FormulaTot=paste0(FormulaTot,"+",ListVar[h])
    }
    }
    #ajout du terme spatial
    DataSIG$FauxGroupe=rep(1,nrow(DataSIG))
    DataSIG$Coord=numFactor(DataSIG$crds_x1,DataSIG$crds_x2)
    FormulaTot=paste(FormulaTot,"+exp(Coord+0|FauxGroupe)")
    #print(FormulaTot)
    FormulaTot=as.formula(FormulaTot)
    #print(FormulaTot)
    
    
    
    
    #test=DataSIG[1:300,]
    Sys.time()
    Mod1=glmmTMB(FormulaTot,data=DataSIG,ziformula=~1,family="nbinom2") # 2 min
    Sys.time()
    save("Mod1",file=paste0(ListSp[i],"_",args,"_Mod1_Routier.RData"))
    Sys.time()
    Pred1=predict(Mod1,newdata=gridSIG,allow.new.levels=TRUE) # 1 min
    #Pred1=predict(Mod1,newdata=gridSIG,allow.new.levels=TRUE,se.fit=T) # 1 min
    Sys.time()
    #polys = as(gridSIG, "SpatialPolygons")
    PredLog=log(Pred1+1,10)
    gridSIG=cbind(gridSIG,PredLog)
    nameGraph=paste0(ListSp[i],"_logActivite_predite_R")
    names(gridSIG)[ncol(gridSIG)]=nameGraph
    
    ScaleAt=c(0:50)/49*max(PredLog)
    
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                     list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
             
    png(paste0(nameGraph,"_",args,".png"))
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    dev.off()
    
    win.metafile(paste0(nameGraph,"_",args,".wmf"))
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    dev.off()
    
}
}  }

PSL_P=subset(PSL,PSL$protocole=="CARRE")

CoordPar=aggregate(PSL_P$participation
                   ,by=c(list(PSL_P$longitude),list(PSL_P$latitude),list(PSL_P$participation),list(PSL_P$Tron))
                   ,FUN=length)


#liste des coordonnées existantes dans ce jeu de données
coordinates(CoordPar) <- c("Group.1", "Group.2")
proj4string(CoordPar) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(gridSIG))
CoordL93=spTransform(CoordPar,CRS.new)
CoordSIG=intersect(CoordL93,gridSIG)
PartSIG=as.data.frame(CoordSIG)


for (i in 1:length(ListSp))
{
  DataSp=subset(SpTronP_P,SpTronP_P$espece==ListSp[i])
  DataSpSL=merge(DataSp,CoordPar,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"))
  NbReplicatsSpatiaux=nlevels(as.factor(paste(DataSpSL$Group.1,DataSpSL$Group.2)))
  print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  #subset des données correspondant à l'espèce i
  DataSpSL_w0=merge(DataSp,PartSIG,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"),all.y=T)
  DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
  
  DataSIG=DataSpSL_w0
  
  if(sum(DataSIG$nb_contacts)>0)
  {
    DataSPos=subset(DataSIG,DataSIG$nb_contacts>0)
    NbReplicatsSpatiaux=nlevels(as.factor(as.character(DataSPos$Coord)))
    #Un premier modèle avec une sélection de variable à la mano  
    #liste variables d'interêt (sélection arbitraire)
    
    if(NbReplicatsSpatiaux>5)
    { 
    NbVar=floor(NbReplicatsSpatiaux/10)  
    
    FormulaTot=paste0("nb_contacts~1")
    if(NbVar>0){
      for (h in 1:min(NbVar,length(ListVar)))
      {
        FormulaTot=paste0(FormulaTot,"+",ListVar[h])
      }
    }
    #ajout du terme spatial
    DataSIG$FauxGroupe=rep(1,nrow(DataSIG))
    DataSIG$Coord=numFactor(DataSIG$crds_x1,DataSIG$crds_x2)
    FormulaTot=paste(FormulaTot,"+exp(Coord+0|FauxGroupe)")
    #print(FormulaTot)
    FormulaTot=as.formula(FormulaTot)
    #print(FormulaTot)
    
    
    
    
    #test=DataSIG[1:300,]
    Sys.time()
    Mod1=glmmTMB(FormulaTot,data=DataSIG,ziformula=~1,family="nbinom2") # 2 min
    Sys.time()
    save("Mod1",file=paste0(ListSp[i],"_",args,"_Mod1_Pedestre.RData"))
    Sys.time()
    Pred1=predict(Mod1,newdata=gridSIG,allow.new.levels=TRUE) # 1 min
    #Pred1=predict(Mod1,newdata=gridSIG,allow.new.levels=TRUE,se.fit=T) # 1 min
    Sys.time()
    #polys = as(gridSIG, "SpatialPolygons")
    PredLog=log(Pred1+1,10)
    gridSIG=cbind(gridSIG,PredLog)
    nameGraph=paste0(ListSp[i],"_logActivite_predite_P")
    names(gridSIG)[ncol(gridSIG)]=nameGraph
    
    ScaleAt=c(0:50)/49*max(PredLog)
    
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    
    png(paste0(nameGraph,"_",args,".png"))
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    dev.off()
    
    win.metafile(paste0(nameGraph,"_",args,".wmf"))
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    dev.off()
    
  }
  }  
}  
#spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
     #        list(axis.line = list(col =  'transparent')),col=NA
      #     ,sp.layout=c(list(FranceWGS84),first=T))

fwrite(as.data.table(gridSIG),paste0("gridSIG_",args,".csv"))
    
  