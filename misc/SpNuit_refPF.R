library(data.table)
library(raster)
library(DT)
library(htmlwidgets)
library(Hmisc)

SelGite=F 
Suffix=""
GI=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/Classes_coord.csv")
#SpNuitTot50=fread("C:/wamp64/www/SpNuit2_50_DataLP_PF_exportTot.csv")
#SpNuitTot90=fread("C:/wamp64/www/SpNuit2_90_DataLP_PF_exportTot.csv")
SpNuitTot=fread("C:/Users/yvesb/Documents/www/SpNuit2Valid_50_PG.csv")
Particip=fread("p_export_forLinux.csv")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
SpeciesList=fread("C:/Users/yvesb/Documents/Tadarida/Tadarida-C/tadaridaC_src/other_inputs/SpeciesList.csv")
IndexGroupe=c("Autre","Sauterelle","Chauve-souris")
SeuilMini=10
RefSaison=data.frame(Mois=c(1:12),Saison=c(rep("Hiver",2),rep("Printemps",3),rep("Ete",4)
                                           ,rep("Automne",2),"Hiver"))
DataCorrection=fread("DataCorrection.csv")


ListSaisons=unique(RefSaison$Saison)


ListHab=unique(GI$Habitat)
#SelHab=NA #NA if no filter
#SelHab="Occitanie"
SelHab=c(NA,ListHab)

DataCorrection$Slope=ifelse(DataCorrection$Slope<0,0,DataCorrection$Slope)
DataCorrection$Slope=ifelse(is.na(DataCorrection$Slope),0,DataCorrection$Slope)
Match31=match(SpNuitTot$espece,DataCorrection$Espece)
SpNuitTot$PostProba=exp(DataCorrection$Slope[Match31]*SpNuitTot$score_max
                        +DataCorrection$Intercept[Match31])/
  (1+exp(DataCorrection$Slope[Match31]*
           SpNuitTot$score_max
         +DataCorrection$Intercept[Match31]))
hist(SpNuitTot$PostProba)
#plot(SpNuitTot$PostProba,SpNuitTot$score_max)
SpNuitTot$PostProba=ifelse(is.na(SpNuitTot$PostProba),1,SpNuitTot$PostProba)
#Sel50=paste(SpNuitTot50$participation,SpNuitTot50$Nuit,SpNuitTot50$espece)
#Sel90=paste(SpNuitTot90$participation,SpNuitTot90$Nuit,SpNuitTot90$espece)

#SpNuitTot=subset(SpNuitTot50,Sel50 %in% Sel90)

if(!SelGite)
{
  Gite=mapply(function(x,y) 
    ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
    ,SiteLoc$commentaire
    ,SiteLoc$localite)
  SiteLoc=subset(SiteLoc,!Gite)
}

SpNuitTot$Mois=as.numeric(substr(SpNuitTot$Nuit,6,7))
#summary(SpNuitTot$Mois)
#barplot(table(SpNuitTot$Mois))
SpNuitTot_backup=SpNuitTot

for (g in 1:length(ListSaisons)){
  print(ListSaisons[g])
  Moisg=subset(RefSaison$Mois,RefSaison$Saison==ListSaisons[g])
  SpNuitTot=subset(SpNuitTot_backup,SpNuitTot_backup$Mois %in% Moisg)
  
  SuffixSave=""
  for (h in 1:length(SelHab))
  {
    print(SelHab[h])
    Suffix=SuffixSave
    if(!is.na(SelHab[h]))
    {
      GISel=subset(GI,GI$Habitat==SelHab[h])
      Suffix=paste0(Suffix,SelHab[h])
      CoordSel=paste(GISel$longitude,GISel$latitude)
      
      
      
      CoordSite=paste(SiteLoc$longitude,SiteLoc$latitude)
      SLSel=subset(SiteLoc,CoordSite %in% CoordSel)
      SLSel=subset(SLSel,SLSel$protocole=="POINT_FIXE")
      
      LocSel=paste(SLSel$site,SLSel$nom)
      LocPar=paste(Particip$site,Particip$point)
      ParSel=subset(Particip,LocPar %in% LocSel)
      
      SpNuit=subset(SpNuitTot,SpNuitTot$participation %in% ParSel$participation)
      
    }else{
      GISel=GI  
      Suffix=paste0(SuffixSave,"Total")
      SpNuit=SpNuitTot
    }
    
    #hist(SpNuit$decalage_debut_coucher,xlim=c(-5000,10000),breaks=200)
    #hist(SpNuit$decalage_fin_lever,xlim=c(-5000,10000),breaks=200)
    
    # SpNuit=subset(SpNuit,(SpNuit$decalage_debut_coucher<0)&
    #                 (SpNuit$decalage_fin_lever<0))
    SpNuit$decalage_debut_coucher=ifelse(SpNuit$decalage_debut_coucher>0,NA
                                         ,SpNuit$decalage_debut_coucher)
    SpNuit$decalage_fin_lever=ifelse(SpNuit$decalage_fin_lever>0,NA
                                     ,SpNuit$decalage_fin_lever)
    
    
    DecMin=mapply(min,SpNuit$min_decalage_lever,SpNuit$min_decalage_coucher)
    SpNuit[,DecMin:=DecMin]
    test=subset(SpNuit,SpNuit$espece=="Rhifer")
    #plot(test$DecMin,test$nb_contacts,xlim=c(-6000,6000),log="y")
    SpNuit=unique(SpNuit,by=c("participation","Nuit","num_micro","espece"))
    
    NbNuit=nlevels(as.factor(paste(SpNuit$participation,SpNuit$Nuit,SpNuit$num_micro)))
    
    i=1
    Q25=vector()
    Q75=vector()
    Q98=vector()
    # Q25nc=vector()
    # Q75nc=vector()
    # Q98nc=vector()
    DM25=vector()
    DM10=vector()
    DM02=vector()
    nbocc=vector()
    MoySiP=vector()
    EtypSiP=vector()
    MoyG=vector()
    MoyGcorr=vector()
    EtypG=vector()
    Espece=vector()
    for (i in 1:nlevels(as.factor(SpNuit$espece)))
    {
      Datasub=subset(SpNuit,SpNuit$espece==levels(as.factor(SpNuit$espece))[i])
      #Data90=subset(SpNuit90,SpNuit90$espece==levels(as.factor(SpNuit$espece))[i])
      #Datasub=subset(Datasub,Datasub$participation %in% Data90$participation)
      #calcul des quantiles d'activit?
      if(nrow(Datasub)>=SeuilMini)
      {
        Espece=c(Espece,levels(as.factor(SpNuit$espece))[i])
        Q25=c(Q25,round(wtd.quantile(Datasub$nb_contacts,0.25,weights=Datasub$PostProba)))
        Q75=c(Q75,round(wtd.quantile(Datasub$nb_contacts,0.75,weights=Datasub$PostProba)))
        Q98=c(Q98,round(wtd.quantile(Datasub$nb_contacts,0.98,weights=Datasub$PostProba)))
        # Q25nc=c(Q25nc,round(quantile(Datasub$nb_contacts,0.25)))
        # Q75nc=c(Q75nc,round(quantile(Datasub$nb_contacts,0.75)))
        # Q98nc=c(Q98nc,round(quantile(Datasub$nb_contacts,0.98)))
        # #print(paste(i,"/",nlevels(as.factor(SpNuit$espece))))
        nbocc=c(nbocc,length(subset(Datasub$nb_contacts,Datasub$PostProba>=max(Datasub$PostProba)/2)))
        MoySiP=c(MoySiP,mean(subset(Datasub$nb_contacts,Datasub$PostProba>=max(Datasub$PostProba)/2)))
        EtypSiP=c(EtypSiP,sd(subset(Datasub$nb_contacts,Datasub$PostProba>=max(Datasub$PostProba)/2)))
        MoyG=c(MoyG,mean(c(Datasub$nb_contacts*Datasub$PostProba,rep(0,NbNuit-nrow(Datasub)))))
        EtypG=c(EtypG,sd(c(Datasub$nb_contacts*Datasub$PostProba,rep(0,NbNuit-nrow(Datasub)))))
        #filtrage des probl?mes d'heures
        Datasub2=subset(Datasub,Datasub$DecMin>(-1800))
        #calcul des quantiles de d?calage de temps minimum avec coucher-lever
        DM25=c(DM25,round(quantile(Datasub2$DecMin,0.25)/60))
        DM10=c(DM10,round(quantile(Datasub2$DecMin,0.10)/60))
        DM02=c(DM02,round(quantile(Datasub2$DecMin,0.02)/60))
        
      }  
    }
    
    Ref=data.frame(Espece,MoyG,EtypG,MoySiP,EtypSiP,Q25,Q75,Q98#,Q25nc,Q75nc,Q98nc
                   ,nbocc
                   ,DM25,DM10,DM02)  
    if(nrow(Ref)>0)
    {
      Ref$Confiance=cut(Ref$nbocc,c(0,40,100,500,Inf),labels=c("Faible","Moderee"
                                                               ,"Bonne","Tres bonne"))
      
      if(Suffix==""){Suffix="Total"}
      Suffix=gsub("/","_",Suffix)
      fwrite(Ref,paste0("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_",ListSaisons[g],"_",Suffix
                        ,"_",Sys.Date(),".csv"),sep=";")
      
      RefShort=subset(Ref,select=c("Espece","nbocc","Q25","Q75","Q98"))
      
      RefSp=merge(RefShort,SpeciesList,by.x="Espece",by.y="Esp")
      RefSp=subset(RefSp,select=c("GroupFR","NomFR","Scientific name","Espece"
                                  ,"nbocc","Q25","Q75","Q98"))
      
      RefSp$Index=match(RefSp$GroupFR,IndexGroupe)
      RefSp=RefSp[with(RefSp
                       ,order(Index,decreasing=T)),]
      RefSp=subset(RefSp,RefSp$nbocc>=SeuilMini)
      names(RefSp)=c("Groupe","Nom francais","Nom scientifique"
                     ,"Code","N","Q25","Q75","Q98")
      print(nrow(RefSp))
      if(nrow(RefSp)>0)
      {
        SummHTML=datatable(RefSp, rownames = FALSE) %>%
          formatStyle(columns = c("Groupe","Nom francais","Nom scientifique"
                                  ,"Code","N","Q25","Q75","Q98"),valueColumns="N", 
                      background = styleInterval(c(40, 100, 500), c("orange", "khaki", "greenyellow", "limegreen"))) 
        
        saveWidget(SummHTML
                   ,paste0("C:/Users/yvesb/Documents/VigieChiro/Referentiels/output-html/refPF_"
                           ,Suffix,"_",Sys.Date()
                           ,".html"))
      }
    }
  }
}
