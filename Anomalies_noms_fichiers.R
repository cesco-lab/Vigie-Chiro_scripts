library(data.table)
library(StreamMetabolism)

#ETAPE 0 - IMPORT DES TABLES
#bien renommer les chemins en fonction de l'ordi utilisé
#et vérifier les versions (date, import complet ou non)

#table "données"
DataTot=fread("C:/wamp64/www/export.txt")
Sys.time()

#table "espèces"
#GroupList=fread("GroupList_HF.csv") 
#table "participations"
Particip=fread("C:/wamp64/www/p_export.csv")
Particip=as.data.frame(Particip)
#table "localités"
SiteLoc=fread("C:/wamp64/www/sites_localites.txt",sep="\t")
#Sites=fread("C:/wamp64/www/sites.txt",sep="\t")


f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

microdroitPF<-function(x)
{
  substr(x,nchar(x)-20,nchar(x)-20)=="1"
  
}

microdroitRP<-function(x)
{
  substr(x,nchar(x)-10,nchar(x)-10)=="1"
  
}

#pour afficher les milisecondes
op <- options(digits.secs=3)
#pour reset
#options(op)


#ETAPE 1 - formattage des tables et de leurs attributs

#merge Localites et participations
PartProt=substr(Particip$site,1,22)
table(PartProt)
SiteLocPF=subset(SiteLoc,SiteLoc$protocole=="POINT_FIXE")
LocaPartPF=merge(SiteLocPF,Particip,by.x=c("site","nom"),by.y=c("site","point"))
#print(paste("nb points manquants :",nrow(PartPF)-nrow(LocaPart),"/",nrow(PartPF)))
SiteLocRP=subset(SiteLoc,SiteLoc$protocole!="POINT_FIXE")
LocaPartRP=merge(SiteLocRP,Particip,by="site",allow.cartesian=TRUE)


#DataTot=subset(DataTot,DataTot$espece!="noise")
colnames(DataTot)[10]="temps_fin"
Sys.time()

#TempsEnregistrement2=sapply(DataTot$donnee,FUN=f2pPF) #long à tourner


#routier-pedestre
PartRP=levels(as.factor(LocaPartRP$participation))

Sys.time()
DataSel2RP=subset(DataTot,DataTot$participation %in% PartRP)
Sys.time()

DataRPNoPrefix=subset(DataSel2RP,substr(DataSel2RP$donnee,1,3)!="Cir")
if(nrow(DataRPNoPrefix)!=0)
  {
  fwrite(DataRPNoPrefix$donnee,"DataRPNoPrefix.csv")
}

rm(NomNumSite, NomAnnee,NomPass,NomTron,NomFF)
PbAPart=vector()
PbT=vector()
PbNTron=vector()
DonneesManquantes=vector()
DroitManquant=vector()
GaucheManquant=vector()
for (i in 1:length(PartRP))
{
  Parti=subset(Particip,Particip$participation==PartRP[i])
  Loci=subset(SiteLocRP,SiteLocRP$site==Parti$site)
    InfoLoci=tstrsplit(Loci$nom," ")
  if(length(InfoLoci)==3)
  {
  TronS=InfoLoci[[2]]
  }else{
    if(length(InfoLoci)==1)
    {
    TronS=InfoLoci[[1]]
    }else{
      TronS=""
    }
  }
  if(nrow(Parti)>1){stop("doublon participation")}
  Sys.time()
  Donneei=subset(DataSel2RP,DataSel2RP$participation==PartRP[i])
  Sys.time()
  if(nrow(Donneei)==0)
  {
    DonneesManquantes=c(DonneesManquantes,PartRP[i])
  }else{
  
  Infoi=tstrsplit(Donneei$donnee,"-")
  
  #check site
  SitePi=tstrsplit(Parti$site,"-")
  NumSiteP=SitePi[[length(SitePi)]]
  testSite=(Infoi[[1]]==paste0("Cir",NumSiteP))
  PbNumSite0=subset(Infoi[[1]],testSite==F)
  if(min(testSite)==F)
  {
    PbNumSite=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
                     ),testSite==F)
    PbNumSite$AR=paste0("Cir",NumSiteP)
    PbNumSite$type="NumSite"
    if(exists("NomNumSite")){NomNumSite=rbind(NomNumSite,PbNumSite)}else{NomNumSite=PbNumSite}
    
    }
  
  #check year
  InfoDi1=tstrsplit(Parti$`date part. debut`,"/")
  Annee1=substr(InfoDi1[[3]],1,4)
  InfoDi2=tstrsplit(Parti$`date part. fin`,"/")
  Annee2=substr(InfoDi1[[3]],1,4)
  if(Annee1!=Annee2){PbAPart=c(PbAPart,PartRP[i])}
  AnneeD=Infoi[[2]]
  testA=(AnneeD==Annee1)
  if(min(testA)==F)
  {
    PbAnnee=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
                     ),testA==F)
    PbAnnee$AR=Annee1
    PbAnnee$type="Annee"
    if(exists("NomAnnee")){NomAnnee=rbind(NomAnnee,PbAnnee)}else{NomAnnee=PbAnnee}
    
    }
  
  #check pass
  PassD=Infoi[[3]]
  testNC=nchar(PassD)
  if(min(testNC)>4)
  {
  testP=(substr(PassD,1,4)=="Pass")
  testN=(!is.na(as.numeric(substr(PassD,5,nchar(PassD)))))
  if((min(testP)==F)|(min(testN)==F))
  {
    PbPass=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
                  ),(testP==F)|(testN==F))
    PbPass$AR="Pass1"
    PbPass$type="Pass"
    if(exists("NomPass")){NomPass=rbind(NomPass,PbPass)}else{NomPass=PbPass}
    
    }
  
  }else{
  PbPass=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
            ),testNC<=4)
  PbPass$AR="Pass1"
  PbPass$type="Pass"
  if(exists("NomPass")){NomPass=rbind(NomPass,PbPass)}else{NomPass=PbPass}
  
  }
  
  #check transect/point
  TronD=Infoi[[4]]
  nTron=nlevels(as.factor(Tron))
  testNC=nchar(TronD)
  if(nTron<5){PbT=c(PbT,PartRP[i])}
  if(min(testNC)>4)
  {
    testT=(substr(TronD,1,4)=="Tron")
    test0=(substr(TronD,5,5)=="0")
    NumTron=as.numeric(substr(TronD,5,nchar(TronD)))
    testN=(!is.na(NumTron))
    testE=match(NumTron,TronS)
    if(max(is.na(testE))==T){PbNTron=c(PbNTron,PartRP[i])}
    if((min(testP)==F)|(max(test0)==T)|(min(testN)==F))
    {
      PbTron=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
                    ),(testP==F)|(test0==T)|(testN==F))
      PbTron$AR="A_definir"
      PbTron$type="Tron"
      if(exists("NomTron")){NomTron=rbind(NomTron,PbTron)}else{NomTron=PbTron}
      
    }
    
  }else{
    PbTron=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
                  ),testNC<=4)
    PbTron$AR="A_definir"
    PbTron$type="Tron"
    if(exists("NomTron")){NomTron=rbind(NomTron,PbTron)}else{NomTron=PbTron}
}
  
  #check channels
  testD=microdroitRP(Donneei$donnee)
  if(max(testD)==F)
  {
    if((Parti$canal_expansion_temps=="DROITE")
       |(Parti$canal_enregistrement_direct=="DROITE"))
      DroitManquant=c(DroitManquant,PartRP[i])
  }
  if(min(testD)==T)
  {
    if((Parti$canal_expansion_temps=="GAUCHE")
       |(Parti$canal_enregistrement_direct=="GAUCHE"))
      GaucheManquant=c(GaucheManquant,PartRP[i])
  }
  #check if double kaleidoscope treatment
  FinFich=substr(Donneei$donnee
                 ,nchar(Donneei$donnee)-9,nchar(Donneei$donnee))
  testFF=(FinFich=="_00000_000")
  if(min(testFF)==T)
  {
    PbFF=as.data.frame(cbind(Donneei$participation,Donneei$donnee))
    PbFF$AR=".wav"
    PbFF$type="FF"
    if(exists("NomFF")){NomFF=rbind(NomFF,PbFF)}else{NomFF=PbFF}
  }
  print(paste(i,Sys.time()))
  }
  #TO DO - check if abnormal splitting (low number of 5_XXX.wav and high number of 'random' _XXX.wav)
  #TO DO - check for abnormal duration (>0.5 sec for expansion, >5 sec for direct)
  }
fwrite(NomNumSite,"AnomalieRP_NomNumSite.csv")
fwrite(NomAnnee,"AnomalieRP_NomAnnee.csv")
fwrite(NomPass,"AnomalieRP_NomPass.csv")
fwrite(NomTron,"AnomalieRP_NomTron.csv")
fwrite(NomFF,"AnomalieRP_NomFF.csv")
fwrite(as.list(PbAPart),"AnomalieRP_PbAPart.csv")
fwrite(as.list(PbT),"AnomalieRP_PbT.csv")   
fwrite(as.list(PbNTron),"AnomalieRP_PbNTron.csv")   
fwrite(as.list(DonneesManquantes),"AnomalieRP_DonneesManquantes.csv")   
fwrite(as.list(DroitManquant),"AnomalieRP_DroitManquant.csv")   
fwrite(as.list(GaucheManquant),"AnomalieRP_GaucheManquant.csv")

#point fixe
ParticipPF=subset(Particip,grepl("Fixe",Particip$site))
PartPF=levels(as.factor(ParticipPF$participation))


Sys.time()
DataSel2PF=subset(DataTot,DataTot$participation %in% PartPF)
Sys.time()

DataPFNoPrefix=subset(DataSel2PF,substr(DataSel2PF$donnee,1,3)!="Car")
if(nrow(DataPFNoPrefix)!=0)
{
  fwrite(DataPFNoPrefix$donnee,"DataPFNoPrefix.csv")
}

rm(NomNumSite, NomAnnee,NomPass,NomTron,NomFF,PbPartPoint)
PbAPart=vector()
PbT=vector()
PbNTron=vector()
DonneesManquantes=vector()
PbPartPoint=Particip[0,]
PrefixeFoireux=vector()
DroitManquant=vector()
GaucheManquant=vector()
for (i in 1:length(PartPF))
{
  Parti=subset(Particip,Particip$participation==PartPF[i])
  Donneei=subset(DataSel2PF,DataSel2PF$participation==PartPF[i])
  Infoi=tstrsplit(Donneei$donnee,"-")
  if (length(Infoi)>3){TronD=Infoi[[4]]}else{TronD=""}
  
  if(Parti$point[1]=="")
  {
    Parti$point=TronD[1]
    PbPartPoint=rbind(PbPartPoint,Parti)
  }
  Loci=subset(SiteLocPF,(SiteLocPF$site==Parti$site)&(SiteLocPF$nom==Parti$point))
  if(nrow(Parti)>1){stop("doublon participation")}
  Sys.time()
  Sys.time()
  if(nrow(Donneei)==0)
  {
    DonneesManquantes=c(DonneesManquantes,PartPF[i])
  }else{
    
    if(length(Infoi)<4)
    {
      PrefixeFoireux=c(PrefixeFoireux,PartPF[i])  
    }else{
      
      #check site
      SitePi=tstrsplit(Parti$site,"-")
      NumSiteP=SitePi[[length(SitePi)]]
      testSite=(Infoi[[1]]==paste0("Car",NumSiteP))
      PbNumSite0=subset(Infoi[[1]],testSite==F)
      if(min(testSite)==F)
      {
        PbNumSite=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
        ),testSite==F)
        PbNumSite$AR=paste0("Car",NumSiteP)
        PbNumSite$type="NumSite"
        if(exists("NomNumSite")){NomNumSite=rbind(NomNumSite,PbNumSite)}else{NomNumSite=PbNumSite}
        
      }
      
      #check year
      InfoDi1=tstrsplit(Parti$date_debut,"/")
      Annee1=substr(InfoDi1[[3]],1,4)
      InfoDi2=tstrsplit(Parti$date_fin,"/")
      Annee2=substr(InfoDi1[[3]],1,4)
      if(Annee1!=Annee2){PbAPart=c(PbAPart,PartPF[i])}
      AnneeD=Infoi[[2]]
      testA=(AnneeD==Annee1)
      if(min(testA[!is.na(testA)])==F)
      {
        PbAnnee=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
        ),testA==F)
        PbAnnee$AR1=Annee1
        PbAnnee$AR1=AnneeD[1]
        PbAnnee$type="Annee"
        if(exists("NomAnnee")){NomAnnee=rbind(NomAnnee,PbAnnee)}else{NomAnnee=PbAnnee}
        
      }
      
      #check pass
      PassD=Infoi[[3]]
      testNC=nchar(PassD)
      if(min(testNC[!is.na(testNC)])>4)
      {
        testP=(substr(PassD,1,4)=="Pass")
        testN=(!is.na(as.numeric(substr(PassD,5,nchar(PassD)))))
        if((min(testP)==F)|(min(testN)==F))
        {
          PbPass=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
          ),(testP==F)|(testN==F))
          PbPass$AR="Pass1"
          PbPass$type="Pass"
          if(exists("NomPass")){NomPass=rbind(NomPass,PbPass)}else{NomPass=PbPass}
        }
        
      }else{
        PbPass=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
        ),testNC<=4)
        PbPass$AR="Pass1"
        PbPass$type="Pass"
        if(exists("NomPass")){NomPass=rbind(NomPass,PbPass)}else{NomPass=PbPass}
        
      }
      
      #check transect/point
      nTron=nlevels(as.factor(TronD))
      testNC=nchar(TronD)
      if(nTron>1)
      {
        PbT=c(PbT,PartPF[i])
        
        
        
        
        
        if(min(testNC)>1)
        {
          testE=match(TronD,Parti$point)
          if(max(is.na(testE))==T)
          {
            PbTron=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
            ),(is.na(testE)))
            PbTron$AR=Parti$point[1]
            PbTron$type="Point"
            if(exists("NomTron")){NomTron=rbind(NomTron,PbTron)}else{NomTron=PbTron}
            
          }
          
        }else{
          PbTron=subset(as.data.frame(cbind(Donneei$participation,Donneei$donnee)
          ),testNC<=1)
          PbTron$AR=Parti$point[1]
          PbTron$type="Point"
          if(exists("NomTron")){NomTron=rbind(NomTron,PbTron)}else{NomTron=PbTron}
        }
      }
      #check if double kaleidoscope treatment
      FinFich=substr(Donneei$donnee
                     ,nchar(Donneei$donnee)-9,nchar(Donneei$donnee))
      testFF=(FinFich=="_00000_000")
      if(min(testFF)==T)
      {
        PbFF=as.data.frame(cbind(Donneei$participation,Donneei$donnee))
        PbFF$AR=".wav"
        PbFF$type="FF"
        if(exists("NomFF")){NomFF=rbind(NomFF,PbFF)}else{NomFF=PbFF}
      }
      print(paste(i,Sys.time()))
    }
    #TO DO - check for absence of recorder and microphone type
    #TO DO - check for abnormal duration (>5 sec)
  }
}
  
fwrite(NomNumSite,"AnomaliePF_NomNumSite.csv")
fwrite(NomAnnee,"AnomaliePF_NomAnnee.csv")
fwrite(NomPass,"AnomaliePF_NomPass.csv")
fwrite(NomTron,"AnomaliePF_NomTron.csv")
fwrite(NomFF,"AnomaliePF_NomFF.csv")
fwrite(as.list(PbAPart),"AnomaliePF_PbAPart.csv")
fwrite(as.list(PbT),"AnomaliePF_PbT.csv")   
fwrite(as.list(PbNTron),"AnomaliePF_PbNTron.csv")   
fwrite(as.list(DonneesManquantes),"AnomaliePF_DonneesManquantes.csv")   
fwrite(as.list(PbPartPoint),"AnomaliePF_PbPartPoint.csv")
fwrite(as.list(PrefixeFoireux),"AnomaliePF_PrefixeFoireux.csv")
