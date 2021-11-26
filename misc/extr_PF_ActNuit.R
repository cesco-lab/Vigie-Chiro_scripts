library(data.table)
library(StreamMetabolism)


WDF=F
if(!exists("FRaw"))
{
  args=""
  args[3]=0 #score threshold
  args[4]="./VigieChiro/Raw/DataLP_PF_export_5ff.csv"
  args[10]="./VigieChiro/Raw/" #output dir
  args[14]=""
  args[12]=""
  args[13]=""
  args[15]=T #sort out doubtful data (probable hardware problems)
  args[16]="C:/wamp64/www/export_validtot201130.txt"
  args[17]=F #correct for validation or not
}
SpeciesList=fread("SpeciesList.csv")


Filter=args[14]
TimeFilterH=args[12]
TimeFilterL=args[13]
ConfOrder=c("POSSIBLE","PROBABLE","SUR")
ConfProba=c(0.5,0.9,0.99)

memory.limit(3210241024*1024)

#ETAPE 0 - IMPORT DES TABLES
#bien renommer les chemins en fonction de l'ordi utilisé

#table "données"
Sys.time()
DataLP=fread(args[4]) # 1e5 lines / sec
Sys.time()

#table "seuils"
#RefSeuils=fread("Referentiel_seuils_tabase3HF_1015France_IdConc_Car.csv")


#table "espèces"
GroupList=fread("SpeciesList.csv") 

LatMin=0
LatMax=90
LongMin=-180
LongMax=180


#pour afficher les milisecondes
op <- options(digits.secs=3)
#pour reset
#options(op)

if(as.logical(args[15])) #sort out doubtful data (mic sensitivity problem)
{
  #ETAPE 0 - tri des participations foireuses (durée séquence Pip)
  #A FAIRE : tri sur le sampling rate
  Sys.time()
  DataPip=subset(DataLP,substr(DataLP$espece,1,3)=="Pip") #3 sec
  Sys.time()
  if(nrow(DataPip)>0)
  {
    DurSeq=DataPip$temps_fin-DataPip$temps_debut
    Q90Pip=aggregate(DurSeq,by=list(DataPip$participation,DataPip$DataMicFinal)
                     ,FUN=function(x) quantile(x,0.9))
    SelQ90Pip=subset(Q90Pip,Q90Pip$x>4.3)
    Sys.time()
    test=match(paste(DataLP$participation,DataLP$DataMicFinal)
               ,paste(SelQ90Pip$Group.1,SelQ90Pip$Group.2)) # 6 sec
    Sys.time()
    DataLP=subset(DataLP,is.na(test)==F)
    Sys.time()
  }
  #filtering out old versions of Tadarida outputs
  decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }
  DataLP$Dec=sapply(DataLP$probabilite,FUN=decimalplaces)
  
  if(nrow(DataLP)>0)
  {
    
    AgDec=aggregate(DataLP$Dec,by=list(DataLP$participation)
                    ,max)
    ParMAJ=subset(AgDec$Group.1,AgDec$x==2)
    DataLP=subset(DataLP,DataLP$participation %in% ParMAJ)
  }
}

if(nrow(DataLP)>0)
{
  #ETAPE 1 - formattage des tables et de leurs attributs
  #ajout des infos temps relatifs / sunrise-sunset
  Sys.time()
  LLJour=unique(cbind(DataLP$latitude,DataLP$longitude,DataLP$DateJour)) # 5 min
  Sys.time()
  #DateSrst=format(as.Date(LLJour[,3],origin = "1970-01-01"),format="%Y/%m/%d")
  DateLP=format(as.Date(LLJour[,3]),format="%Y-%m-%d")
  Sys.time()
  Srst=mapply(sunrise.set,as.numeric(LLJour[,1]),as.numeric(LLJour[,2]),DateLP) #50 sec
  Sys.time()
  SrstD=as.data.frame(t(Srst))
  NightDur=3600*24+(as.numeric(SrstD$sunrise)-
                      as.numeric(SrstD$sunset))
  
  DataSrst=as.data.frame(cbind(LLJour,SrstD,NightDur))
  colnames(DataSrst)=c("latitude","longitude","DateJour","sunrise","sunset","nightdur")
  DataSrst$latitude=as.numeric(as.character(DataSrst$latitude))
  DataSrst$longitude=as.numeric(as.character(DataSrst$longitude))
  
  
  gc()
  Sys.time()
  DataLPS=merge(DataLP,DataSrst,by=c("latitude","longitude","DateJour"))
  Sys.time()
  rm(DataLP)
  
  Sys.time()
  Decst=DataLPS$TempsEnregistrement2-as.numeric(DataLPS$sunset)
  Sys.time()
  #recale par rapport au coucher de soleil du bon jour
  DecstP=Decst+3600*24*(Decst<(-6*3600))  # 2 min
  Sys.time()
  DataLPS[,DecstP:=DecstP]
  Sys.time()
  
  Decsr=as.numeric(DataLPS$sunrise)-DataLPS$TempsEnregistrement2
  #recale par rapport au lever de soleil du bon jour
  DecsrP=Decsr+3600*24*(Decsr<(-6*3600)) # 2 min
  Sys.time()
  DataLPS[,DecsrP:=DecsrP]
  Sys.time()
  
  Prop1=Decst/DataLPS$nightdur  #recale par rapport au lever de soleil du bon jour
  Prop2=1-Decsr/DataLPS$nightdur  #recale par rapport au lever de soleil du bon jour
  Prop=ifelse(abs(Prop1-0.5)<abs(Prop2-0.5),Prop1,Prop2)
  Sys.time()
  DataLPS[,PropH:=Prop]
  Sys.time()
  
  
  if(Filter=="sunrise")
  {
    DataLPS=subset(DataLPS,DataLPS$DecsrP<=as.numeric(TimeFilterH))
    DataLPS=subset(DataLPS,DataLPS$DecsrP>as.numeric(TimeFilterL))
  }
  
  if(Filter=="sunset")
  {
    DataLPS=subset(DataLPS,DataLPS$DecstP<=as.numeric(TimeFilterH))
    DataLPS=subset(DataLPS,DataLPS$DecstP>as.numeric(TimeFilterL))
  }
  
  if(Filter=="time_int")
  {
    DataLPS=subset(DataLPS,DataLPS$PropH<=as.numeric(TimeFilterH))
    DataLPS=subset(DataLPS,DataLPS$PropH>as.numeric(TimeFilterL))
  }
  
  if(nrow(DataLPS)>0)
  {
    
    if(exists("RefSeuils"))
    {
      #merge avec espèce pour tri selon seuil
      #simplifie la table groupe pour ne pas alourdir la grosse table Data...
      #simplifie la table groupe pour ne pas alourdir la grosse table Data...
      GroupSimpl=data.frame(espece=GroupList$Esp,nom=GroupList$`Scientific name`
                            ,groupe=GroupList$Group)
      GroupRef=merge(GroupSimpl,RefSeuils,by.x="espece",by.y="Espece")
      
      Sys.time()
      DataLPSG=merge(DataLPS,GroupRef,by="espece")
      Sys.time()
      test=match(DataLPS$espece,GroupRef$espece)
      SpManquante=subset(DataLPS,is.na(test))
      table(SpManquante$espece)
      rm(DataLPS)
      
      
      #ColS=match(args[3],colnames(DataLPSG))
      Sys.time()
      ColSeuil=match(args[3],names(DataLPSG))
      Fiable=(DataLPSG$probabilite>DataLPSG[,..ColSeuil])
      Sys.time()
      table(Fiable,DataLPSG$espece)
      Sys.time()
      DataFiable=subset(DataLPSG,as.logical(Fiable)) # 10 sec
      Sys.time()
      rm(DataLPSG) # 30 sec
      #test=DataFiable[1:100000,]
    }else{
      GroupSimpl=data.frame(espece=GroupList$Esp,nom=GroupList$`Scientific name`
                            ,groupe=GroupList$Group)
      DataLPSG=merge(DataLPS,GroupSimpl,by="espece")
      DataFiable=subset(DataLPSG,DataLPSG$probabilite>=(as.numeric(args[3])/100)) # 10 sec
    if(as.numeric(args[3])>=50){
      DataFiable=subset(DataFiable,!as.logical(DataFiable$doubtful))
    }
      }
    
    if(WDF)
    {
      fwrite(DataFiable,paste0(args[10],"/S_",args[4]))
    }
    
    test=subset(DataFiable,DataFiable$participation=="5824922150f6b2000d3a2687")
    table(as.character(test$espece))
    
    #correct for validation
    if(args[17])
    {
      EVT=fread(args[16])
      testo=match(EVT$obs.espece,SpeciesList$Esp)
      #table(subset(EVT$obs.espece,is.na(testo)))
      testv=match(EVT$valid.espece,SpeciesList$Esp)
      #table(subset(EVT$valid.espece,is.na(testv)))
      #head(EVT$obs.espece)
      #head(SpeciesList$Nesp2[testo])
      EVT$obs.espece=SpeciesList$Nesp2[testo]
      EVT$valid.espece=SpeciesList$Nesp2[testv]
      EVT[is.na(EVT)]=""
      
      DataFiable$espece=as.character(DataFiable$espece)
      
      DataCorr=DataFiable[0,]
      #test=match("5824922150f6b2000d3a2687",unique(DataFiable$participation))
      for (a in 1:length(unique(DataFiable$participation)))
      {
        Dataa=subset(DataFiable
                     ,DataFiable$participation==
                       unique(DataFiable$participation)[a])
        EVTa=subset(EVT
                    ,EVT$participation==unique(DataFiable$participation)[a])
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
            
            if(nrow(Datab)==0){Datab=rbind(Datab,EVTb,use.names=T,fill=T)}
            EVTb$error=(EVTb$ovsp!=unique(EVTa$espece)[b])
            if(max(EVTb$error)==0)#no error
            {
              #DataCorr=rbind(DataCorr,Datab)
            }else{
              if(min(EVTb$error)==1) #all errors
              {
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
            Datab=subset(Datab,select=names(DataCorr))
            DataCorr=rbind(DataCorr,Datab)
          }
          
        }else{
          DataCorr=rbind(DataCorr,Dataa)
        }
        
      }
      DataFiable=DataCorr
    }
    
    Sys.time()
    DataPF_ActNuit=aggregate(DataFiable$donnee
                             ,by=list(DataFiable$participation
                                      ,DataFiable$DateNuit
                                      ,DataFiable$DataMicFinal
                                      ,DataFiable$groupe
                                      ,DataFiable$espece
                             )
                             ,FUN=length) # 15 min
    Sys.time()
    DataND=subset(DataFiable,!DataFiable$doubtful)
    
    DataPF_ActNuitND=aggregate(DataND$donnee
                             ,by=list(DataND$participation
                                      ,DataND$DateNuit
                                      ,DataND$DataMicFinal
                                      ,DataND$groupe
                                      ,DataND$espece
                             )
                             ,FUN=length) # 15 min
    
    Sys.time()
    DataPF_ScoreNuit=aggregate(DataFiable$probabilite
                               ,by=list(DataFiable$participation
                                        ,DataFiable$DateNuit
                                        ,DataFiable$DataMicFinal
                                        ,DataFiable$groupe
                                        ,DataFiable$espece
                               )
                               ,FUN=max) # 15 min
    Sys.time()
    DataPF_MinSt=aggregate(DataFiable$DecstP
                           ,by=list(DataFiable$participation
                                    ,DataFiable$DateNuit
                                    ,DataFiable$DataMicFinal
                                    ,DataFiable$groupe
                                    ,DataFiable$espece
                           )
                           ,FUN=min)
    Sys.time()
    DataPF_MinSr=aggregate(DataFiable$DecsrP
                           ,by=list(DataFiable$participation
                                    ,DataFiable$DateNuit
                                    ,DataFiable$DataMicFinal
                                    ,DataFiable$groupe
                                    ,DataFiable$espece
                           )
                           ,FUN=min)
    Sys.time()
    DataPF_SpNuit=cbind(DataPF_ActNuit
                        ,DataPF_ScoreNuit$x
                        ,DataPF_MinSt$x
                        ,DataPF_MinSr$x)
    DataPF_ActNuitND$nb_contacts_nd=DataPF_ActNuitND$x
    DataPF_ActNuitND$x=NULL
    
    DataPF_SpNuit=merge(DataPF_SpNuit,DataPF_ActNuitND,by=c("Group.1"
                                                            ,"Group.2"
                                                            ,"Group.3"
                                                            ,"Group.4"
                                                            ,"Group.5")
                        ,all.x=T)
    DataPF_SpNuit$nb_contacts_nd[is.na(DataPF_SpNuit$nb_contacts_nd)]=0     
    colnames(DataPF_SpNuit)=c("participation","Nuit","num_micro","groupe"
                              ,"espece"
                              ,"nb_contacts",
                              "score_max","min_decalage_coucher"
                              ,"min_decalage_lever","nb_contacts_nd")
    
    
    #fwrite(DataPF_SpNuit,paste0(args[10],"/SpNuit",Filter,TimeFilter,basename(args[4])))
    
    Sys.time()
    DataDMinSr=aggregate(DataFiable$DecsrP
                         ,by=list(DataFiable$participation
                                  ,DataFiable$DateNuit
                                  ,DataFiable$DataMicFinal)
                         ,FUN=min) # 20 sec
    Sys.time()
    DataDMaxSr=aggregate(DataFiable$DecsrP
                         ,by=list(DataFiable$participation
                                  ,DataFiable$DateNuit
                                  ,DataFiable$DataMicFinal)
                         ,FUN=max) # 20 sec
    Sys.time()
    DataDMinSt=aggregate(DataFiable$DecstP
                         ,by=list(DataFiable$participation
                                  ,DataFiable$DateNuit
                                  ,DataFiable$DataMicFinal)
                         ,FUN=min)
    Sys.time()
    DataDMaxSt=aggregate(DataFiable$DecstP
                         ,by=list(DataFiable$participation
                                  ,DataFiable$DateNuit
                                  ,DataFiable$DataMicFinal)
                         ,FUN=max)
    Sys.time()
    DataDecPNM=cbind(DataDMinSr,DataDMaxSr$x,DataDMinSt$x,DataDMaxSt$x)
    colnames(DataDecPNM)=c("participation","Nuit","num_micro"
                           ,"decalage_fin_lever"
                           ,"decalage_debut_lever"
                           ,"decalage_debut_coucher"
                           ,"decalage_fin_coucher")
    
    DataPF_SpNuit2=merge(DataPF_SpNuit,DataDecPNM
                         ,by=c("participation","Nuit","num_micro"))
    
    if(args[17]){ #compute checked confidences
      EVTi=subset(EVT,EVT$participation %in% DataPF_SpNuit2$participation)
      EVTi$confo=match(EVTi$obs.proba,ConfOrder)
      EVTo=subset(EVTi,!is.na(EVTi$confo))
      if(nrow(EVTo)>0)
      {
        EVToa=aggregate(EVTo$confo,by=c(list(EVTo$participation)
                                        ,list(EVTo$espece))
                        ,max)
        EVToa$x=ConfOrder[EVToa$x]
        names(EVToa)=c("participation","espece","confiance_observateur")
        DataPF_SpNuit2=merge(DataPF_SpNuit2,EVToa
                             ,by=c("participation","espece"),all.x=T)
      }
      EVTi$confv=match(EVTi$valid.proba,ConfOrder)
      EVTv=subset(EVTi,!is.na(EVTi$confv))
      if(nrow(EVTv)>0)
      {
        EVTva=aggregate(EVTv$confv,by=c(list(EVTv$participation)
                                        ,list(EVTv$espece))
                        ,max)
        EVTva$x=ConfOrder[EVTva$x]
        names(EVTva)=c("participation","espece","confiance_validateur")
        DataPF_SpNuit2=merge(DataPF_SpNuit2,EVTva
                             ,by=c("participation","espece"),all.x=T)
      }
      DataPF_SpNuit2[is.na(DataPF_SpNuit2)]=""
      
      NameFile=paste0(args[10],"/SpNuit2Valid_",Filter,TimeFilterL,"_"
                      ,TimeFilterH,"_",args[3],"_",basename(args[4]))
      
    }else{
      NameFile=paste0(args[10],"/SpNuit2_",Filter,TimeFilterL,"_"
                      ,TimeFilterH,"_",args[3],"_",basename(args[4]))
      
    }
    

    fwrite(DataPF_SpNuit2,NameFile,sep=";")
    
  }
}

