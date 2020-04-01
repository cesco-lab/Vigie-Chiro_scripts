library(data.table)
library(raster)
library(DT)
library(htmlwidgets)

SelGite=F 
Suffix=""
GI=fread("Classes_coord.csv")
SpNuit=fread("C:/wamp64/www/SpNuit2_50_DataLP_PF_exportTot.csv")
SpNuit90=fread("C:/wamp64/www/SpNuit2_90_DataLP_PF_exportTot.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
SpeciesList=fread("SpeciesList.csv")
IndexGroupe=c("Autre","Sauterelle","Chauve-souris")
SeuilMini=10

ListHab=unique(GI$Habitat)
#SelHab=NA #NA if no filter
#SelHab="Occitanie"
SelHab=c(NA,ListHab)


if(!SelGite)
{
  Gite=mapply(function(x,y) 
    ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
    ,SiteLoc$commentaire
    ,SiteLoc$localite)
  SiteLoc=subset(SiteLoc,!Gite)
}


SuffixSave=Suffix
for (h in 1:length(SelHab))
{
print(SelHab[h])
    Suffix=SuffixSave
  if(!is.na(SelHab[h]))
{
  GISel=subset(GI,GI$Habitat==SelHab[h])
  CoordSel=paste(GISel$longitude,GISel$latitude)
  Suffix=paste0(Suffix,SelHab[h])
  }



CoordSite=paste(SiteLoc$longitude,SiteLoc$latitude)
SLSel=subset(SiteLoc,CoordSite %in% CoordSel)
SLSel=subset(SLSel,SLSel$protocole=="POINT_FIXE")

LocSel=paste(SLSel$site,SLSel$nom)
LocPar=paste(Particip$site,Particip$point)
ParSel=subset(Particip,LocPar %in% LocSel)

SpNuit=subset(SpNuit,SpNuit$participation %in% ParSel$participation)

hist(SpNuit$decalage_debut_coucher,xlim=c(-5000,10000),breaks=200)
hist(SpNuit$decalage_fin_lever,xlim=c(-5000,10000),breaks=200)

SpNuit=subset(SpNuit,(SpNuit$decalage_debut_coucher<0)&
                (SpNuit$decalage_fin_lever<0))

DecMin=mapply(min,SpNuit$min_decalage_lever,SpNuit$min_decalage_coucher)
SpNuit[,DecMin:=DecMin]
test=subset(SpNuit,SpNuit$espece=="Rhifer")
plot(test$DecMin,test$nb_contacts,xlim=c(-6000,6000),log="y")

NbNuit=nlevels(as.factor(paste(SpNuit$participation,SpNuit$Nuit,SpNuit$num_micro)))

i=1
Q25=vector()
Q75=vector()
Q98=vector()
DM25=vector()
DM10=vector()
DM02=vector()
nbocc=vector()
MoySiP=vector()
EtypSiP=vector()
MoyG=vector()
EtypG=vector()
Espece=vector()
for (i in 1:nlevels(as.factor(SpNuit$espece)))
{
  Datasub=subset(SpNuit,SpNuit$espece==levels(as.factor(SpNuit$espece))[i])
  Data90=subset(SpNuit90,SpNuit90$espece==levels(as.factor(SpNuit$espece))[i])
  Datasub=subset(Datasub,Datasub$participation %in% Data90$participation)
  #calcul des quantiles d'activité
  if(nrow(Datasub)>0)
  {
    Espece=c(Espece,levels(as.factor(SpNuit$espece))[i])
    Q25=c(Q25,round(quantile(Datasub$nb_contacts,0.25)))
    Q75=c(Q75,round(quantile(Datasub$nb_contacts,0.75)))
    Q98=c(Q98,round(quantile(Datasub$nb_contacts,0.98)))
    print(paste(i,"/",nlevels(as.factor(SpNuit$espece))))
    nbocc=c(nbocc,nrow(Datasub))
    MoySiP=c(MoySiP,mean(Datasub$nb_contacts))
    EtypSiP=c(EtypSiP,sd(Datasub$nb_contacts))
    MoyG=c(MoyG,mean(c(Datasub$nb_contacts,rep(0,NbNuit-nrow(Datasub)))))
    EtypG=c(EtypG,sd(c(Datasub$nb_contacts,rep(0,NbNuit-nrow(Datasub)))))
    #filtrage des problèmes d'heures
    Datasub2=subset(Datasub,Datasub$DecMin>(-1800))
    #calcul des quantiles de décalage de temps minimum avec coucher-lever
    DM25=c(DM25,round(quantile(Datasub2$DecMin,0.25)/60))
    DM10=c(DM10,round(quantile(Datasub2$DecMin,0.10)/60))
    DM02=c(DM02,round(quantile(Datasub2$DecMin,0.02)/60))
    
  }  
}

Ref=data.frame(Espece,MoyG,EtypG,MoySiP,EtypSiP,Q25,Q75,Q98,nbocc
          ,DM25,DM10,DM02)  

if(Suffix==""){Suffix="Total"}
fwrite(Ref,paste0("./VigieChiro/Referentiels/refPF_",Suffix
                              ,".csv"),sep=";")

RefShort=subset(Ref,select=c("Espece","nbocc","Q25","Q75","Q98"))

RefSp=merge(RefShort,SpeciesList,by.x="Espece",by.y="Esp")
RefSp=subset(RefSp,select=c("GroupFR","NomFR","Scientific name","Espece"
                            ,"nbocc","Q25","Q75","Q98"))

RefSp$Index=match(RefSp$GroupFR,IndexGroupe)
RefSp=RefSp[with(RefSp
                       ,order(Index,decreasing=T)),]
RefSp=subset(RefSp,RefSp$nbocc>SeuilMini)
names(RefSp)=c("Groupe","Nom francais","Nom scientifique"
               ,"Code","N","Q25","Q75","Q98")
print(nrow(RefSp))
SummHTML=datatable(RefSp, rownames = FALSE) %>%
  formatStyle(columns = c("Groupe","Nom francais","Nom scientifique"
                          ,"Code","N","Q25","Q75","Q98"),valueColumns="N", 
              background = styleInterval(c(40, 100, 500), c("orange", "khaki", "greenyellow", "limegreen"))) 

saveWidget(SummHTML
           ,paste0("C:/Users/Yves Bas/Documents/VigieChiro/Referentiels/output-html/refPF_"
                   ,Suffix
                           ,".html"))
}