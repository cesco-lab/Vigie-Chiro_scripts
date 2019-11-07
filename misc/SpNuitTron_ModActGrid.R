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

arg1="DataRP_SpTronSeuil90"
arg2="DataPF_SpNuit2_Seuil90"
TriDirectUniquement=T

#Un premier modèle avec une sélection de variable à la mano  
#liste variables d'interêt (sélection arbitraire)
ListVar=c("n_CLC_23","n_CLC_122","n_CLC_311","n_CLC_112","n_CLC_312","norm_PP","nrm_DtA","n_CLC_242","norm_PC","n_CLC_141","n_CLC_323","norm_Lm","n_CLC_221","n_CLC_411","n_CLC_313","n_CLC_243","n_CLC_211","n_CLC_241")


#recupération des données chiros
DataCPLRP=fread(paste0(arg1,".csv"))
DataCPLPF=fread(paste0(arg2,".csv"))


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

SpTron=DataCPLRP
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


ListSp=c("Barbar","Eptser","Nyclei","Nycnoc","Pipkuh","Pippip")


#SiteLoc
#PourTron=tstrsplit(SiteLoc$nom," ")
SiteLoc$Tron=sapply(SiteLoc$nom
                    ,FUN=function(x) if(substr(x,1,1)=="T"){tstrsplit(x," ")[[2]]}else{x})

SiteLocU=unique(SiteLoc,by=c("site","Tron"))
SiteLocU=subset(SiteLocU,SiteLocU$protocole!="POINT_FIXE")

PSL=merge(Particip,SiteLocU,by="site",allow.cartesian=TRUE)
PSL=subset(PSL,PSL$protocole!="POINT_FIXE")

ListPar_RP=levels(as.factor(SpTronP$participation))
PSL=subset(PSL,PSL$participation %in% ListPar_RP)

PSL_R=subset(PSL,PSL$protocole=="ROUTIER")





ListPar_R=levels(as.factor(SpTron_R$participation))


CoordPar_R=aggregate(PSL_R$participation
                   ,by=c(list(PSL_R$longitude),list(PSL_R$latitude),list(PSL_R$participation),list(PSL_R$Tron))
                   ,FUN=length)


#FranceWGS84 <- SpatialPolygons(list(Polygons(list(Polygon(FranceWGS84)), 1))) 


#liste des coordonnées existantes dans ce jeu de données
coordinates(CoordPar_R) <- c("Group.1", "Group.2")
proj4string(CoordPar_R) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(gridSIG))
CoordL93=spTransform(CoordPar_R,CRS.new)
CoordSIG=intersect(CoordL93,gridSIG)

gridSIG$nbP_R=log(colSums(gContains(gridSIG,CoordL93, byid = TRUE))+1)

png("Couverture_Protocole_Routier.png")
print(spplot(gridSIG,zcol="nbP_R",main="Couverture Protocole Routier",par.settings =
  list(axis.line = list(col =  'transparent'))
  ,colorkey=F))
dev.off()


win.metafile("Couverture_Protocole_Routier.wmf")
print(spplot(gridSIG,zcol="nbP_R",main="Couverture Protocole Routier",par.settings =
               list(axis.line = list(col =  'transparent'))
             ,colorkey=F))
dev.off()

setNames(res, x@data$NAME_1)
PartSIG_R=as.data.frame(CoordSIG)
#spplot(CoordSIG,zcol="norm_Lm")

FranceWGS84=spTransform(FranceD,CRS(proj4string(CoordPar_R)))


PSL_P=subset(PSL,PSL$protocole=="CARRE")

CoordPar_P=aggregate(PSL_P$participation
                     ,by=c(list(PSL_P$longitude),list(PSL_P$latitude),list(PSL_P$participation),list(PSL_P$Tron))
                     ,FUN=length)

#liste des coordonnées existantes dans ce jeu de données
coordinates(CoordPar_P) <- c("Group.1", "Group.2")
proj4string(CoordPar_P) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(gridSIG))
CoordL93=spTransform(CoordPar_P,CRS.new)
CoordSIG=intersect(CoordL93,gridSIG)


gridSIG$nbP_P=log(colSums(gContains(gridSIG,CoordL93, byid = TRUE))+1)

png("Couverture_Protocole_Pedestre.png")
print(spplot(gridSIG,zcol="nbP_P",main="Couverture Protocole Pedestre",par.settings =
               list(axis.line = list(col =  'transparent'))
             ,colorkey=F))
dev.off()


win.metafile("Couverture_Protocole_Pedestre.wmf")
print(spplot(gridSIG,zcol="nbP_P",main="Couverture Protocole Pedestre",par.settings =
               list(axis.line = list(col =  'transparent'))
             ,colorkey=F))
dev.off()



PartSIG_P=as.data.frame(CoordSIG)
#spplot(CoordSIG,zcol="norm_Lm")

ListPar=levels(as.factor(DataCPLPF$participation))
SelPar=subset(Particip,Particip$participation %in% ListPar)
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
CoordPar_PF=aggregate(SelParSL$participation
                   ,by=c(list(SelParSL$longitude),list(SelParSL$latitude),list(SelParSL$participation))
                   ,FUN=length)

coordinates(CoordPar_PF) <- c("Group.1", "Group.2")
proj4string(CoordPar_PF) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(gridSIG))
CoordL93=spTransform(CoordPar_PF,CRS.new)


gridSIG$nbP_PF=log(colSums(gContains(gridSIG,CoordL93, byid = TRUE))+1)

png("Couverture_Protocole_Point_Fixe.png")
print(spplot(gridSIG,zcol="nbP_PF",main="Couverture Protocole Point Fixe",par.settings =
               list(axis.line = list(col =  'transparent'))
             ,colorkey=F))
dev.off()


win.metafile("Couverture_Protocole_Point_Fixe.wmf")
print(spplot(gridSIG,zcol="nbP_P",main="Couverture Protocole Pedestre",par.settings =
               list(axis.line = list(col =  'transparent'))
             ,colorkey=F))
dev.off()

gridSIG$Couv=gridSIG$nbP_R+gridSIG$nbP_P+gridSIG$nbP_PF
MaxScale=6
ScaleAtP=c(c(0:49)/49*MaxScale,Inf)

png("Couverture_Tous_Protocoles.png")

print(spplot(gridSIG,zcol="Couv",main="Couverture Tous Protocoles",par.settings =
               list(axis.line = list(col =  'transparent'))
             ,colorkey=F,at=ScaleAtP))

dev.off()


win.metafile("Couverture_Tous_Protocoles.wmf")

print(spplot(gridSIG,zcol="Couv",main="Couverture Tous Protocoles",par.settings =
               list(axis.line = list(col =  'transparent'))
             ,colorkey=F,at=ScaleAtP))

dev.off()

CoordSIG=intersect(CoordL93,gridSIG)
PartSIG_PF=as.data.frame(CoordSIG)

testP=match(SpTronP$participation,CoordPar_R$Group.3)
SpTronP_P=subset(SpTronP,is.na(testP))
SpTronP_R=subset(SpTronP,is.na(testP)==F)


for (i in 1:length(ListSp))
{
  DataSp=subset(SpTronP_R,SpTronP_R$espece==ListSp[i])
  DataSpSL=merge(DataSp,CoordPar_R,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"))
  NbReplicatsSpatiaux=nlevels(as.factor(paste(DataSpSL$Group.1,DataSpSL$Group.2)))
  print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  #subset des données correspondant à l'espèce i
  DataSpSL_w0=merge(DataSp,PartSIG_R,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"),all.y=T)
  DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
  
    DataSIG_R=DataSpSL_w0
  
    DataSp=subset(SpTronP_P,SpTronP_P$espece==ListSp[i])
    DataSpSL=merge(DataSp,CoordPar_P,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"))
    NbReplicatsSpatiaux=nlevels(as.factor(paste(DataSpSL$Group.1,DataSpSL$Group.2)))
    print(paste(ListSp[i],nrow(DataSp),Sys.time()))
    #subset des données correspondant à l'espèce i
    DataSpSL_w0=merge(DataSp,PartSIG_P,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"),all.y=T)
    DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
    
    DataSIG_P=DataSpSL_w0
    
    
    DataSp=subset(DataCPLPF,DataCPLPF$espece==ListSp[i])
    DataSpSL=merge(DataSp,SelParSL,by="participation")
    
    print(paste(ListSp[i],nrow(DataSp),Sys.time()))
    #subset des données correspondant à l'espèce i
    #  if(NbReplicatsSpatiaux>length(ListVar)*30)  
    # {
    
    DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
    DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
    
    DataSIG_PF=merge(DataSpSL_w0,PartSIG_PF
                  ,by.x=c("participation")
                  ,by.y=c("Group.3"))
    
    DataSIG_R$Routier=1
    DataSIG_P$Pedestre=1
    DataSIG=rbind(DataSIG_PF,DataSIG_R,fill=T)
    DataSIG=rbind(DataSIG,DataSIG_P,fill=T)
    DataSIG$Routier[is.na(DataSIG$Routier)]=0
    DataSIG$Pedestre[is.na(DataSIG$Pedestre)]=0
    
    
    
    
  if(sum(DataSIG$nb_contacts)>0)
  {
    DataSPos=subset(DataSIG,DataSIG$nb_contacts>0)
    NbReplicatsSpatiaux=nlevels(as.factor(as.character(DataSPos$Coord)))
    #Un premier modèle avec une sélection de variable à la mano  
    #liste variables d'interêt (sélection arbitraire)
    if(NbReplicatsSpatiaux>30)
    { 
      
    NbVar=floor(NbReplicatsSpatiaux/20)-2  
      
    FormulaTot=paste0("nb_contacts~Routier+Pedestre")
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
    Mod1=glmmTMB(FormulaTot,data=DataSIG,ziformula=~1,family="nbinom2") # 4 min
    Sys.time()
    save("Mod1",file=paste0(ListSp[i],"_",arg1,arg2,"_Mod1.RData"))
    
    gridSIG$Routier=0
    gridSIG$Pedestre=0    
    Sys.time()
    #Pred1=predict(Mod1,newdata=gridSIG,allow.new.levels=TRUE) # 1 min
    Pred1=predict(Mod1,newdata=gridSIG,allow.new.levels=TRUE,se.fit=T) # 1 min
    Sys.time()
    #polys = as(gridSIG, "SpatialPolygons")
    PredLog=log(Pred1+1,10)
    PredAcc=1/Pred1$se.fit*Pred1$fit
    summary(PredAcc)
    hist(log(PredAcc-0.43),breaks=50)
    gridSIG$PredAcc=log(PredAcc-0.43)
    gridSIG=cbind(gridSIG,PredLog)
    
    
    
    MaxScale=quantile(PredAcc,0.95)
    if(is.na(MaxScale)){MaxScale=0.1}
    #ScaleAt=c(c(0:49)/49*MaxScale,Inf)
    
    png(paste0("PredAcc_",i,"_",arg1,arg2,".png"))
        print(spplot(gridSIG,zcol="PredAcc",main="precision",par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions()))
    
    dev.off()
    
    win.metafile(paste0("PredAcc_",i,"_",arg1,arg2,".wmf"))
    print(spplot(gridSIG,zcol="PredAcc",main="precision",par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions()))
    
    dev.off()
    
        gridSIG=cbind(gridSIG,PredLog)
        
    nameGraph=paste0(ListSp[i],"_logActivite_predite_3protocoles")
    names(gridSIG)[ncol(gridSIG)]=nameGraph
    
    
    MaxScale=quantile(subset(PredLog,PredLog>0.1),0.95)
    if(is.na(MaxScale)){MaxScale=0.1}
    ScaleAt=c(c(0:49)/49*MaxScale,Inf)
    
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                     list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
             
    png(paste0(nameGraph,"_",arg1,arg2,".png"))
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    dev.off()
    
    win.metafile(paste0(nameGraph,"_",arg1,arg2,".wmf"))
    print(spplot(gridSIG,zcol=nameGraph,main=nameGraph,par.settings =
                   list(axis.line = list(col =  'transparent'))
                 ,col.regions=get_col_regions(),at=ScaleAt))
    dev.off()
    
}
  }  
    }

fwrite(as.data.table(gridSIG),paste0("gridSIG_",arg1,arg2,".csv"))