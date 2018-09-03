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

TriDirectUniquement=T

args="DataRP_SpTronSeuil90"
#recupération des données chiros
SpTron=fread(paste0(args,".csv"))
#récupération des données participation
Particip=fread("C:/wamp64/www/p_export.txt",encoding="UTF-8")
#récupération des localités
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")


#France_departement
FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
Sys.time()

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


#SiteLoc
#PourTron=tstrsplit(SiteLoc$nom," ")
SiteLoc$Tron=sapply(SiteLoc$nom
            ,FUN=function(x) if(substr(x,1,1)=="T"){tstrsplit(x," ")[[2]]}else{x})

SiteLocU=unique(SiteLoc,by=c("site","Tron"))

PSL=merge(Particip,SiteLocU,by="site",allow.cartesian=TRUE)
PSL=subset(PSL,PSL$protocole!="POINT_FIXE")
PSL_R=subset(PSL,PSL$protocole=="ROUTIER")

CoordPar=aggregate(PSL_R$participation
                   ,by=c(list(PSL_R$longitude),list(PSL_R$latitude),list(PSL_R$participation),list(PSL_R$Tron))
                   ,FUN=length)

coordinates(CoordPar) <- c("Group.1", "Group.2")
proj4string(CoordPar) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(FranceD))
CoordL93=spTransform(CoordPar,CRS.new)
CoordFranceD=intersect(CoordL93,FranceD)
PartFranceD=as.data.frame(CoordFranceD)



#liste des coordonnées existantes dans ce jeu de données

ListSp=levels(as.factor(SpTronP$espece))
SpTronP$Tron=as.factor(SpTronP$Tron)
testP=match(SpTronP$participation,CoordPar$Group.3)
SpTronP_P=subset(SpTronP,is.na(testP))
SpTronP_R=subset(SpTronP,is.na(testP)==F)




for (i in 1:length(ListSp))
{
  DataSp=subset(SpTronP_R,SpTronP_R$espece==ListSp[i])
  DataSpSL=merge(DataSp,CoordPar,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"))
  NbReplicatsSpatiaux=nlevels(as.factor(paste(DataSpSL$Group.1,DataSpSL$Group.2)))
  print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  #subset des données correspondant à l'espèce i
    DataSpSL_w0=merge(DataSp,PartFranceD,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"),all.y=T)
    DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
    
  AggDep=aggregate(DataSpSL_w0$nb_contacts,by=list(DataSpSL_w0$DépARTEM0),FUN=mean)
  RepDep=aggregate(DataSpSL_w0$nb_contacts,by=list(DataSpSL_w0$DépARTEM0),FUN=length)
 # hist(log(AggDep$x+1),breaks=50)
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
    CarteActDep2$Act_map[(CarteActDep2$x.y<10)]=(-999)
    
    CarteActDep2$IntAct=findInterval(CarteActDep2$Act_map,vec=breaks)
    CarteActDep2$colAct=cols[CarteActDep2$IntAct]
win.metafile(file=paste0(ListSp[i],"_",args,"Routier_Dep_Act.wmf"))
  print(  spplot(CarteActDep2,zcol="IntAct",col.regions=cols,colorkey=F,par.settings =
             list(axis.line = list(col =  'transparent')),main=paste(ListSp[i],"/ Routier - Activité"))
)
           #Sys.sleep(0.5)
dev.off() 
fwrite(ARDep,paste0(ListSp[i],"_",args,"Routier_ARDep.csv"))
 
print(paste(ListSp[i],NbReplicatsSpatiaux))
   }


PSL_P=subset(PSL,PSL$protocole=="CARRE")

CoordPar=aggregate(PSL_P$participation
                   ,by=c(list(PSL_P$longitude),list(PSL_P$latitude),list(PSL_P$participation),list(PSL_P$Tron))
                   ,FUN=length)

coordinates(CoordPar) <- c("Group.1", "Group.2")
proj4string(CoordPar) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS(proj4string(FranceD))
CoordL93=spTransform(CoordPar,CRS.new)
CoordFranceD=intersect(CoordL93,FranceD)
PartFranceD=as.data.frame(CoordFranceD)



for (i in 1:length(ListSp))
{
  DataSp=subset(SpTronP_P,SpTronP_P$espece==ListSp[i])
  DataSpSL=merge(DataSp,CoordPar,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"))
  NbReplicatsSpatiaux=nlevels(as.factor(paste(DataSpSL$Group.1,DataSpSL$Group.2)))
  print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  #subset des données correspondant à l'espèce i
  DataSpSL_w0=merge(DataSp,PartFranceD,by.x=c("participation","Tron"),by.y=c("Group.3","Group.4"),all.y=T)
  DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
  
  AggDep=aggregate(DataSpSL_w0$nb_contacts,by=list(DataSpSL_w0$DépARTEM0),FUN=mean)
  RepDep=aggregate(DataSpSL_w0$nb_contacts,by=list(DataSpSL_w0$DépARTEM0),FUN=length)
  # hist(log(AggDep$x+1),breaks=50)
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
  CarteActDep2$Act_map[(CarteActDep2$x.y<10)]=(-999)
  
  CarteActDep2$IntAct=findInterval(CarteActDep2$Act_map,vec=breaks)
  CarteActDep2$colAct=cols[CarteActDep2$IntAct]
win.metafile(file=paste0(ListSp[i],"_",args,"Pedestre_Dep_Act.wmf"))
  print(  spplot(CarteActDep2,zcol="IntAct",col.regions=cols,colorkey=F,par.settings =
                   list(axis.line = list(col =  'transparent')),main=paste(ListSp[i],"/ Pedestre - Activité"))
  )
  #Sys.sleep(0.5)
  dev.off() 
  fwrite(ARDep,paste0(ListSp[i],"_",args,"Pedestre_ARDep.csv"))
  
  print(paste(ListSp[i],NbReplicatsSpatiaux))
}

 