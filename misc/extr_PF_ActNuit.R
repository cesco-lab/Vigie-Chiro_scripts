library(data.table)
library(StreamMetabolism)

Tri=T
WDF=F
#args=""
#args[3]=90
#args[4]="D:/VigieChiro/Raw/DataLP_PF_export_5d3.csv"
#args[10]="D:/VigieChiro/Raw/"
#Tri=F
#WDF=T
#args[14]=""
#args[12]=""
#args[13]=""
Filter=args[14]
TimeFilterH=args[12]
TimeFilterL=args[13]


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

if(Tri)
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
  DataSrst=as.data.frame(cbind(LLJour,SrstD))
  colnames(DataSrst)=c("latitude","longitude","DateJour","sunrise","sunset")
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
  
  if(Filter=="sunrise")
  {
    DataLPS=subset(DataLPS,DataLPS$DecsrP<TimeFilterH)
    DataLPS=subset(DataLPS,DataLPS$DecsrP>TimeFilterL)
  }
  
  if(Filter=="sunset")
  {
    DataLPS=subset(DataLPS,DataLPS$DecstP<TimeFilterH)
    DataLPS=subset(DataLPS,DataLPS$DecstP>TimeFilterL)
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
      DataFiable=subset(DataLPSG,DataLPSG$probabilite>(as.numeric(args[3])/100)) # 10 sec
    }
    
    if(WDF)
    {
      fwrite(DataFiable,paste0(args[10],"/S_",args[4]))
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
    DataPF_SpNuit=cbind(DataPF_ActNuit,DataPF_MinSt$x,DataPF_MinSr$x)
    colnames(DataPF_SpNuit)=c("participation","Nuit","num_micro","groupe","espece"
                              ,"nb_contacts","min_decalage_coucher","min_decalage_lever")
    
    
    
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
    
    fwrite(DataPF_SpNuit2,paste0(args[10],"/SpNuit2_",Filter,TimeFilterL,"_"
                                 ,TimeFilterH,"_",args[3],"_",basename(args[4])))
    
  }
}

