library(data.table)
#ETAPE 0 - IMPORT DES TABLES
#bien renommer les chemins en fonction de l'ordi utilisé
#et vérifier les versions (date, import complet ou non)

LatMin=0
LatMax=90
LongMin=-180
LongMax=180
FDataTot="D:/VigieChiro/Raw"
#DataTot=fread("C:/wamp64/www/export.txt")
#table "participations"
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
#table "localités"
SiteLoc=fread("C:/wamp64/www/sites_localites.txt",sep="\t")
Sectorized=T
EVT=fread("C:/wamp64/www/export_validtot201130.txt")
Valid=T
SpeciesList=fread("SpeciesList.csv")
ConfOrder=c("POSSIBLE","PROBABLE","SUR")
ConfProba=c(0.5,0.9,0.99)

microdroitRP<-function(x)
{
  substr(x,nchar(x)-10,nchar(x)-10)=="1"
}


Particip=as.data.frame(Particip)

SiteLocRP=subset(SiteLoc,SiteLoc$protocole!="POINT_FIXE")
#aggrégation au tronçon
SiteLocRP$Tron=sapply(SiteLocRP$nom,FUN=function(x) if(nchar(x)>2){substr(x,3,nchar(x)-2)}else{x})
SiteLocRP$Secteur=sapply(SiteLocRP$nom,FUN=function(x) if(nchar(x)>2){substr(x,nchar(x),nchar(x))}else{3})
#crée une table avec une seule ligne par tronçon (un tronçon est donc réduit à son 3ème secteur)
SiteLocRP_TronU=subset(SiteLocRP,SiteLocRP$Secteur=="3")


#pour afficher les milisecondes
op <- options(digits.secs=3)
#pour reset
#options(op)

#merge Localites et participations
PartProt=substr(Particip$site,1,22)
table(PartProt)
PartRP=subset(Particip,PartProt!="Vigiechiro - Point Fix")
table(PartRP$canal_expansion_temps)
if(Sectorized)
{
  LocaPart=merge(PartRP,SiteLocRP,by="site")
}else{
LocaPart=merge(PartRP,SiteLocRP_TronU,by="site")
}



if(dir.exists(FDataTot))
{
  Fe=list.files(FDataTot,pattern="export_",full.names=T)
  Fe=subset(Fe,substr(basename(Fe),1,1)=="e")
  ListData=list()
  for (i in 1:length(Fe))
  {
    Tempe=fread(Fe[i])
    ListData[[i]]=subset(Tempe,Tempe$participation %in% PartRP$participation)
  print(Fe[i])
    }
  DataTot=rbindlist(ListData)
}else{
DataTot=fread(FDataTot)
}

Sys.time()
DataRP=subset(DataTot,substr(DataTot$donnee,1,3)=="Cir")
Sys.time()
rm(DataTot)

if(ncol(DataRP)-sum(is.na(match(colnames(DataRP),"temps_debut")))==2)
{colnames(DataRP)[10]="temps_fin"}


if(Valid)
{
    testo=match(EVT$obs.espece,SpeciesList$Esp)
    #table(subset(EVT$obs.espece,is.na(testo)))
    testv=match(EVT$valid.espece,SpeciesList$Esp)
    #table(subset(EVT$valid.espece,is.na(testv)))
    #head(EVT$obs.espece)
    #head(SpeciesList$Nesp2[testo])
    EVT$obs.espece=SpeciesList$Nesp2[testo]
    EVT$valid.espece=SpeciesList$Nesp2[testv]
    EVT[is.na(EVT)]=""
    
    DataRP$espece=as.character(DataRP$espece)
    
    DataCorr=DataRP[0,]
    for (a in 1:length(unique(DataRP$participation)))
    {
      Dataa=subset(DataRP
                   ,DataRP$participation==
                     unique(DataRP$participation)[a])
      EVTa=subset(EVT
                  ,EVT$participation==unique(DataRP$participation)[a])
      if(nrow(EVTa)>0){
        #stop("test")
        EVTa$ovsp=ifelse(EVTa$valid.espece=="",EVTa$obs.espece
                         ,EVTa$valid.espece)
        EVTa$ovconf=ifelse(EVTa$valid.proba=="",EVTa$obs.proba
                           ,EVTa$valid.proba)
        EVTa$ovconf=match(EVTa$ovconf,ConfOrder)
        EVTa$ovconf=ConfProba[EVTa$ovconf]
        print(substr(EVTa$donnee[1],1,15))
        print(unique(EVTa$espece))
        Dataa2=subset(Dataa,!(Dataa$espece %in% EVTa$espece))
        DataCorr=rbind(DataCorr,Dataa2)
        
        for (b in 1:length(unique(EVTa$espece)))
        {
          Datab=subset(Dataa,Dataa$espece==unique(EVTa$espece)[b])
          EVTb=subset(EVTa,EVTa$espece==unique(EVTa$espece)[b])
          names(EVTb)[10]="temps_fin"
          #if("cigale" %in% EVTb$valid.espece){stop("cigale")}
          #if("cigale" %in% Datab$espece){stop("cigaleD")}
          
          if(nrow(Datab)==0){
            #stop("test Datab vide")
            Datab=rbind(Datab,EVTb,use.names=T,fill=T)
            }
          EVTb$error=(EVTb$ovsp!=unique(EVTa$espece)[b])
          if(max(EVTb$error)==0)#no error
          {
              }else{
            #stop ("test error")
            if(min(EVTb$error)==1) #all errors
            {
              #stop("all")
              Datab$probabilite=0
            }else{ #some errors
              #stop("t")
              pfalse=mean(subset(EVTb$probabilite,EVTb$error==1))
              ptrue=mean(subset(EVTb$probabilite,EVTb$error==0))
              if(ptrue>pfalse){ #normal case - possible threshold value
                Databt=subset(Datab
                              ,Datab$probabilite>mean(c(pfalse,ptrue)))
                Databf=subset(Datab
                              ,Datab$probabilite<=mean(c(pfalse,ptrue)))
                Databf$probabilite=0
                Datab=rbind(Databt,Databf)
              }else{ #weird case - no threshold to be found 
                #stop("weird")
                #- only validated records retained
                Datab$probabilite=0
              }
            }
            
          }
          lEVTb=match(paste(EVTb$donnee,EVTb$espece)
                      ,paste(Datab$donnee,Datab$espece)
          )
          if(max(is.na(lEVTb))!=1){
            #stop("test")
            Datab$espece[lEVTb]=EVTb$ovsp
            Datab$probabilite[lEVTb]=EVTb$ovconf
          }
          Datab$ovconf=NULL
          Datab$ovsp=NULL
          DataCorr=rbind(DataCorr,Datab)
        }
        
      }else{
                DataCorr=rbind(DataCorr,Dataa)
      }
      
    }
    DataRP=DataCorr
  }
  




#DataTot=subset(DataTot,DataTot$espece!="noise")
LocaPartData=as.factor(substr(DataRP$donnee,1,27)) #récupération de l'identifiant du point/tronçon
Sys.time()
Datamicro=as.character(sapply(DataRP$donnee,FUN=microdroitRP)) # récupération du numéro du micro (4 min)
Sys.time()

DataSel2=cbind(DataRP,LocaPartData,Datamicro)
rm(DataRP)
rm(LocaPartData)
rm(Datamicro)


FileInfo=as.data.table(tstrsplit(DataSel2$donnee,"-"))
DataSel2$Session=substr(FileInfo$V4,5,nchar(FileInfo$V4))
TimeSec=as.data.table(tstrsplit(FileInfo$V5,"_"))
TimeSec=as.data.frame(TimeSec)
#test=(is.na(as.numeric(TimeSec$V4)))
#DataBug=subset(DataSel2,test)
#TimeBug=subset(TimeSec,test)
Sys.time()
DataSel2$TimeTron=mapply(FUN=function(x,y,z) if(is.na(z))
  {as.numeric(x)+as.numeric(y)/1000}else{as.numeric(y)+as.numeric(z)/1000}
  ,TimeSec$V2,TimeSec$V3,TimeSec$V4) # 8 sec
Sys.time()
gc()
TimeMaxTron=aggregate(DataSel2$TimeTron,by=c(list(DataSel2$participation)
                                             ,list(DataSel2$Session))
                      ,max)
DataSel2=merge(DataSel2,TimeMaxTron,by.x=c("participation","Session")
               ,by.y=c("Group.1","Group.2"))
DataSel2$Secteur=ceiling(DataSel2$TimeTron/DataSel2$x*5)
DataSel2$Secteur=as.character(pmax(1,DataSel2$Secteur))
DataSel2$Pedestre=(substr(DataSel2$donnee,10,10)=="-")
DataSel2$Pedestre=ifelse(DataSel2$Pedestre
                         ,(substr(DataSel2$donnee,5,5)!="-"),F)
DataSel2$Secteur=ifelse(DataSel2$Pedestre,"3",DataSel2$Secteur)


Sys.time()
if(Sectorized)
{
DataLP_RP=merge(DataSel2,LocaPart
                ,by.x=c("participation","Session","Secteur")
                ,by.y=c("participation","Tron","Secteur")) # 2 sec
Sys.time()
}else{
  DataLP_RP=merge(DataSel2,LocaPart
                  ,by.x=c("participation","Session")
                  ,by.y=c("participation","Tron")) # 2 sec
  
}
rm(DataSel2)
gc()

#purge des champs inutiles pour gagner de la mémoire (à remonter ?)
ListePurge=c("proprietaire","num site",
             "observateur.x",
             "email.y",
             "id_protocole",
             "protocole",
             "localite",
             "date.y",
             "observateur.y",
             "nb_wav",
             "nb_ta",
             "nb_tc",
             "dif_wav_ta",
             "pourc_dif",
             "trait_fin",
             "detecteur_enregistreur_numero_serie",
             "canal_expansion_temps",
             "canal_enregistrement_direct",
             "micro0_numero_serie",
             "micro1_numero_serie",
             "commentaire")
Sys.time()
DataLP_RP[,(ListePurge):=NULL]
Sys.time()

Suffix=""
if(Sectorized){
Suffix=paste0(Suffix,"_Sectorized")


}
if(Valid){
  Suffix=paste0(Suffix,"_Valid")
  
}

Sys.time()
fwrite(DataLP_RP,paste0("DataLP_RP",Suffix,".csv"),row.names=F) # 1 sec
Sys.time()
#DataLP_R=subset(DataLP_RP,grepl("Routier",DataLP_RP$site))
#DataLP_P=subset(DataLP_RP,!grepl("Routier",DataLP_RP$site))

#fwrite(DataLP_R,"DataLP_R.csv",row.names=F) # 1 sec
#fwrite(DataLP_P,"DataLP_P.csv",row.names=F) # 1 sec
