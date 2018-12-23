library(data.table)
args="C:/wamp64/www/export.txt"
args[2]="pourHop" #suffixe export
Loc=F

SelRP=F
SelPF=T

SelLongLat=F
LatMin=43.43
LatMax=43.73
LongMin=4.02
LongMax=4.50

OutNoise=T
SelValid=F
FiltrValid="SUR" 

SelU=T
ListUM=fread("C:/wamp64/www/liste_utilisateurs_mail.txt",h=F)

SelYear=T
ListYear="2018"

Data=fread(args[1])
if(names(Data)[9]=="temps_debut")
{
names(Data)[10]="temps_fin"
}

SpeciesList=fread("SpeciesList.csv")
SpecShort=subset(SpeciesList,select=c("Esp","Scientific name","NomFR","GroupFR"))


SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
Utilisateurs=fread("C:/wamp64/www/utilisateurs.txt")


if(SelU)
{
  U=subset(Utilisateurs,Utilisateurs$email %in% ListUM$V1)
  PU=subset(Particip,Particip$idobservateur %in% U$identifiant)
  Data=subset(Data,Data$participation %in% PU$participation)
}

if(SelYear)
{
  PYear=subset(Particip,substr(Particip$date_debut,7,10) %in% ListYear)
  Data=subset(Data,Data$participation %in% PYear$participation)
  }


if(Loc){
  

ListPar=levels(as.factor(Data$participation))
SelPar=subset(Particip,Particip$participation %in% ListPar)

if(SelLongLat)
{
  SLSel=subset(SiteLoc,(SiteLoc$latitude>LatMin)
               &(SiteLoc$latitude<LatMax)
               &(SiteLoc$longitude>LongMin)
               &(SiteLoc$longitude<LongMax))
}else{
  SLSel=SiteLoc
}

if(SelRP){
SLSelR=subset(SLSel,SLSel$protocole=="ROUTIER")
#trondetail=tstrsplit(SLSelRP$nom," ")
SelParSLR=merge(SLSelR,SelPar,by="site",allow.cartesian=TRUE)
DataRP=subset(Data,substr(Data$donnee,1,3)=="Cir")
RPdetail=tstrsplit(DataRP$donnee,"-")
DataRP$Tron=substr(RPdetail[[4]],5,nchar(RPdetail[[4]]))
RPtempsdetail=tstrsplit(RPdetail[[5]],"_")
DataRP$section=pmin(floor(as.numeric(RPtempsdetail[[3]])/70)+1,5)
DataRP$TronR=paste("T",DataRP$Tron,DataRP$section,sep=" ")

DataSLR=merge(DataRP,SelParSLR,by.x=c("participation","TronR"),by.y=c("participation","nom"))



SLSelP=subset(SLSel,SLSel$protocole=="CARRE")
#trondetail=tstrsplit(SLSelRP$nom," ")
SelParSLP=merge(SLSelP,SelPar,by="site",allow.cartesian=TRUE)


#test=subset(SelPar,SelPar$participation=="5a076e96fe80cc000db43afb")
#test=subset(SLSelP,SLSelP$id_site=="564c90b7eea470000e1d19a3")

DataSLP=merge(DataRP,SelParSLP,by.x=c("participation","Tron"),by.y=c("participation","nom"))

}

if(SelPF){
SLSelPF=subset(SLSel,SLSel$protocole=="POINT_FIXE")

SelParSLPF=merge(SLSelPF,SelPar,by.x=c("site","nom"),by.y=c("site","point"))

DataSLPF=merge(Data,SelParSLPF,by="participation")

DataSLPFP=rbind(DataSLPF,DataSLP,fill=T,use.names=T)
}


DataSL=rbind(DataSLPFP,DataSLR,fill=T,use.names=T)
}else{
  DataSL=Data
  
}

if(SelValid){
if(sum(grepl("valid.espece",names(DataSL)))==1)
{
DataSp1=merge(DataSL,SpecShort,by.x="valid.espece",by.y="Esp")
DatasansV=subset(DataSL,DataSL$valid.espece=="")
DataSp2=merge(DatasansV,SpecShort,by.x="obs.espece",by.y="Esp")
DataSp=rbind(DataSp1,DataSp2)
}else{
  DataSp=merge(DataSL,SpecShort,by.x="espece",by.y="Esp")
}
  DataSp=subset(DataSp,DataSp$valid.proba %in% FiltrValid)
  
  }else{
  DataSp=merge(DataSL,SpecShort,by.x="espece",by.y="Esp")
}
  
if(OutNoise)
{
  is.upper <- "[A-Z]"
  result <- grepl(pattern = is.upper, x = substr(DataSp$NomFR,1,1))
  DataSp=subset(DataSp,result)
}


NomExport=paste0("./VigieChiro/Exports/",substr(basename(args[1]),1,nchar(basename(args[1]))-4),"_",args[2],"_SL.csv")
write.csv2(DataSp,NomExport,row.names=F)


