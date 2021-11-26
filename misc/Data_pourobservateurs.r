library(data.table)
args="C:/wamp64/www/SpNuit2_5090_DataLP_PF_exportTot.csv"
args="E:/VigieChiro/Raw"
#args="DataRP_SpTronSeuil90.csv"
args[2]="pourKevinFEE" #suffixe export
#args[2]="pourRBFG" #suffixe export
Shiny=F
SelLoc=F
SelRP=F
SelPF=T
SelSite=F
ListSite=c("200285","81126","940058","811229","670799","910034"
           ,"670799","670778","930002","310277","770085","650322","201628"
           ,"770107")
SelLongLat=F
LatMin=48.13
LatMax=48.63
LongMin=2.16
LongMax=2.98
OutNoise=F
SelValid=F
FiltrValid="SUR" 
SelU=F
Open=F
#ListUM=fread("C:/wamp64/www/liste_utilisateurs_mail.txt",h=F)
ListUM=data.frame(V1="bandchiro@gmail.com")
SelYear=F
ListYear="2018"
SelSp=F
FiltSp="Eptser"
SelPar=T
ListPar=data.frame(init=1)
ListPar=fread("C:/wamp64/www/p_feeForKevin.csv",h=T)
#ListPar$participation="5f781f1d055889001015099c"
SpeciesList=fread("SpeciesList.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8"
               ,sep=";")
Utilisateurs=fread("C:/wamp64/www/utilisateurs_prev.txt")

SpecShort=subset(SpeciesList,select=c("Esp","Scientific name","NomFR","GroupFR"))

if(dir.exists(args[1]))
{
  FDatalist=list.files(args[1],full.names=T,pattern="export")
  FDatalist=subset(FDatalist,substr(basename(FDatalist),1,6)=="export")
  FDatalist=subset(FDatalist,!grepl("validtot",FDatalist))  
}else{
  FDatalist=args[1]
}


Datalist=list()
for (i in 1:length(FDatalist))
{
  Data=fread(FDatalist[i])
  if(nrow(Data)>0)
  {
    
    if(ncol(Data)>8)
    {
      if(names(Data)[9]=="temps_debut")
      {
        names(Data)[10]="temps_fin"
      }
    }
    
    
    if(SelU)
    {
      U=subset(Utilisateurs,Utilisateurs$email %in% ListUM$V1)
      PU=subset(Particip,Particip$idobservateur %in% U$identifiant)
      Data=subset(Data,Data$participation %in% PU$participation)
    }
    
    if(SelPar)
    {
      Data=subset(Data,Data$participation %in% ListPar$participation)
    }
    
    if(SelYear)
    {
      PYear=subset(Particip,substr(Particip$date_debut,7,10) %in% ListYear)
      Data=subset(Data,Data$participation %in% PYear$participation)
    }
    
    
    if(SelLoc)
    {
      
      
      ListPar=levels(as.factor(Data$participation))
      ParSel=subset(Particip,Particip$participation %in% ListPar)
      
      if(SelSite)
      {
        if(SelRP)
        {
          ProperSiteName=c(paste0("Vigiechiro - Pédestre-"
                                  ,ListSite)
                           ,paste0("Vigie-chiro - Routier-"
                                   ,ListSite))
          
        }else{
          ProperSiteName=vector()
        }
        if(SelPF)
        {
          ProperSiteName=c(ProperSiteName,paste0("Vigiechiro - Point Fixe-",ListSite))
        }
        SLSel=subset(SiteLoc,SiteLoc$site %in% ProperSiteName)
      }else{
        if(SelLongLat)
        {
          SLSel=subset(SiteLoc,(SiteLoc$latitude>LatMin)
                       &(SiteLoc$latitude<LatMax)
                       &(SiteLoc$longitude>LongMin)
                       &(SiteLoc$longitude<LongMax))
        }else{
          SLSel=SiteLoc
        }
        
      }
      
      if(SelRP){
        if(sum(grepl("donnee",names(Data)))>0)
        {
          
          SLSelR=subset(SLSel,SLSel$protocole=="ROUTIER")
          #trondetail=tstrsplit(SLSelRP$nom," ")
          SelParSLR=merge(SLSelR,ParSel,by="site",allow.cartesian=TRUE)
          DataRP=subset(Data,substr(Data$donnee,1,3)=="Cir")
          RPdetail=tstrsplit(DataRP$donnee,"-")
          DataRP$Tron=substr(RPdetail[[4]],5,nchar(RPdetail[[4]]))
          RPtempsdetail=tstrsplit(RPdetail[[5]],"_")
          DataRP$section=pmin(floor(as.numeric(RPtempsdetail[[3]])/70)+1,5)
          DataRP$TronR=paste("T",DataRP$Tron,DataRP$section,sep=" ")
          
          DataSLR=merge(DataRP,SelParSLR,by.x=c("participation","TronR"),by.y=c("participation","nom"))
          
          
          
          SLSelP=subset(SLSel,SLSel$protocole=="CARRE")
          #trondetail=tstrsplit(SLSelRP$nom," ")
          SelParSLP=merge(SLSelP,ParSel,by="site",allow.cartesian=TRUE)
          
          
          #test=subset(ParSel,ParSel$participation=="5a076e96fe80cc000db43afb")
          #test=subset(SLSelP,SLSelP$id_site=="564c90b7eea470000e1d19a3")
          
          DataSLP=merge(DataRP,SelParSLP,by.x=c("participation","Tron"),by.y=c("participation","nom"))
        }else{
          SelParSLR=merge(SLSel,ParSel,by="site",allow.cartesian=TRUE)
          trondetail=tstrsplit(SelParSLR$nom," ")
          if(length(trondetail)>1)
          {
            SelParSLR$Tron=as.numeric(trondetail[[2]])
          }else{
            SelParSLR$Tron=as.numeric(trondetail[[1]])
            
          }
          DataSLR=merge(Data,SelParSLR,by=c("participation","Tron"))
          DataSLP=data.frame()
          
        }
        
      }else{
        DataSLR=data.frame()
        DataSLP=data.frame()
        
      }
      
      if(SelPF){
        SLSelPF=subset(SLSel,SLSel$protocole=="POINT_FIXE")
        
        SelParSLPF=merge(SLSelPF,ParSel,by.x=c("site","nom"),by.y=c("site","point"))
        
        DataSLPF=merge(Data,SelParSLPF,by="participation")
        
        DataSLPFP=rbind(DataSLPF,DataSLP,fill=T,use.names=T)
        DataSL=rbind(DataSLPFP,DataSLR,fill=T,use.names=T)
        DataSp=DataSL
      }else{
        DataSL=DataSLR
        DataSp=DataSL
      }
      
      
      
    }else{
      DataSp=merge(Data,Particip,by="participation")
      DataSp=merge(DataSp,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
    }
    
    if(nrow(DataSp)>0)
    {  
      if(SelValid){
        if(sum(grepl("valid.espece",names(DataSL)))==1)
        {
          DataSp1=merge(DataSp,SpecShort,by.x="valid.espece",by.y="Esp")
          DatasansV=subset(DataSp,DataSp$valid.espece=="")
          DataSp2=merge(DatasansV,SpecShort,by.x="obs.espece",by.y="Esp")
          DataSp=rbind(DataSp1,DataSp2)
        }else{
          DataSp=merge(DataSp,SpecShort,by.x="espece",by.y="Esp")
        }
        DataSp=subset(DataSp,DataSp$valid.proba %in% FiltrValid)
        
      }else{
        DataSp=merge(DataSp,SpecShort,by.x="espece",by.y="Esp")
      }
      
      if(OutNoise)
      {
        is.upper <- "[A-Z]"
        result <- grepl(pattern = is.upper, x = substr(DataSp$NomFR,1,1))
        DataSp=subset(DataSp,result)
      }
      
      if(SelSp)
      {
        DataSp=subset(DataSp,grepl(FiltSp,DataSp$espece))
      }
      
    }
    Datalist[[i]]=DataSp
    print(paste(basename(FDatalist[i]),Sys.time(),nrow(Data)))  
  }
}
DataSp=rbindlist(Datalist,fill=T,use.names=T)


if(Open)
{
  UO=subset(Utilisateurs$email,Utilisateurs$confidentiel=="non")
  DataSp=subset(DataSp,DataSp$email %in% UO)
}

DataSp$email=NULL
DataSp$observateur.y=NULL

if(Shiny)
{
  DirExport=paste0("./VigieChiro/Exports/",args[2])
  dir.create(DirExport)
  LP=unique(DataSp$participation)
  for (j in 1:length(LP))
  {
    Dataj=subset(DataSp,DataSp$participation==LP[j])
    Dataj$tadarida_taxon_autre=""
    Datajs=subset(Dataj,select=c("donnee","temps_debut","temps_fin",
"frequence","espece","probabilite","tadarida_taxon_autre","obs.espece"
,"obs.proba","valid.espece","valid.proba"))
    
    colnames(Datajs)=c("nom du fichier","temps_debut","temps_fin"
,"frequence_mediane","tadarida_taxon","tadarida_probabilite"
,"tadarida_taxon_autre","observateur_taxon","observateur_probabilite"
,"validateur_taxon","validateur_probabilite")
  Namejs=paste0(DirExport,"/participation-",LP[j],"-observations.csv")
  fwrite(Datajs,Namejs,sep=";")
    }
  
}else{
  NomExport=paste0("./VigieChiro/Exports/",substr(basename(args[1]),1,nchar(basename(args[1]))-4),"_",args[2],"_SL.csv")
  fwrite(DataSp,NomExport,row.names=F,sep=";")
  
}
