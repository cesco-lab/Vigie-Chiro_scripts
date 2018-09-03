library(StreamMetabolism)

library(data.table)
library(MASS)
pasdetemps=7
DataTot=fread("C:/wamp64/www/export_tot171228.txt",sep="\t")


Particip=fread("C:/wamp64/www/p_export.txt",encoding="UTF-8")
Particip=as.data.frame(Particip)


ETV=fread("C:/wamp64/www/export_validtot171228.txt",sep="\t")
colnames(ETV)[10]="temps_fin"


f2p <- function(x) 
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

SpeciesList=fread("SpeciesList.csv") #version modifiée par Fabien


BatSpecies=subset(SpeciesList,SpeciesList$Group=="bat")


#DataTot=subset(DataTot,DataTot$espece!="noise")
colnames(DataTot)[10]="temps_fin"

DataBat=merge(DataTot,BatSpecies,by.x="espece",by.y="Nesp")

write.csv2(table(DataBat$espece),"DataEsp.csv")


test=match(DataTot$donnee,ETV$donnee)
DataV=subset(DataTot,is.na(test)==F)

ETV_prop=aggregate(ETV$donnee,by=c(list(ETV$proprietaire),list(ETV$obs.espece),list(ETV$valid.espece)),FUN=length)
ETV_prop2=subset(ETV_prop,(ETV_prop$Group.2!="")&(ETV_prop$Group.3!=""))
Verif=(ETV_prop2$Group.2==ETV_prop2$Group.3)
Verif_prop=aggregate(Verif,by=c(list(ETV_prop2$Group.1),list(ETV_prop2$Group.2)),FUN=mean)
Pb_Id=subset(Verif_prop,(Verif_prop$x<0.95)&(Verif_prop$Group.1!="Yves Bas"))

ETV_PI=merge(ETV,Pb_Id,by.x=c("proprietaire","obs.espece"),by.y=c("Group.1","Group.2"),all.x=T)
ETV_filtree=subset(ETV_PI,(ETV_PI$x=="")|(ETV_PI$valid.espece)!=""|(ETV_PI$proprietaire=="Yves Bas"))

for (i in 1:nrow(ETV_filtree))
{
  if(ETV_filtree$valid.espece[i]==""){ETV_filtree$valid.espece[i]=ETV_filtree$obs.espece[i]}
  print(i)
}

write.csv(ETV_filtree,"ETV_filtree.csv",row.names=F)

#ReP=merge(ReQMfilt,Particip,by.x="Group.1",by.y="participation")



SiteLoc=fread("C:/wamp64/www/sites_localites.txt",sep="\t")

SelSite=subset(SiteLoc,(SiteLoc$latitude>43.3)&(SiteLoc$latitude<44.4)&(SiteLoc$longitude>(3.2))&(SiteLoc$longitude<(4.9)))

#SelSite=subset(SiteLoc,SiteLoc$protocole=="POINT_FIXE")
#SelSite=as.data.frame(SiteLoc)


#SelListSite=aggregate(cbind(SelSite$longitude,SelSite$latitude),by=list(SelSite$site),FUN=mean)

#plot(SelSite$longitude,SelSite$latitude)

PartSelG=merge(Particip,SelSite,by.x=c("site","point"),by.y=c("site","nom"))

Mois=substr(PartSelG$'date part. debut',4,5)
Annee=substr(PartSelG$'date part. debut',7,10)

NbP_SiteAnnee=aggregate(PartSelG$participation,by=c(list(PartSelG$site),list(Annee)),FUN=length)
NbS_Annee=aggregate(NbP_SiteAnnee$Group.1,by=list(NbP_SiteAnnee$Group.2),FUN=length)
Date1=as.Date(substr(PartSelG$`date part. debut`,1,10),format="%d/%m/%Y")
Date2=as.Date(substr(PartSelG$`date part. fin`,1,10),format="%d/%m/%Y")
DurPF=Date2-Date1
PartSelG=cbind(PartSelG,Mois,Annee,DurPF)
write.csv(PartSelG,"PartSelG.csv",row.names=F)





SpCible="Pippyg"
#calcul de taux e faux négatifs
ETVSp1=subset(ETV_filtree,ETV_filtree$valid.espece==SpCible)
ETVSp2=subset(ETV_filtree,(ETV_filtree$valid.espece=="")&(ETV_filtree$obs.espece==SpCible))
ETVSp3=rbind(ETVSp1,ETVSp2)
test2=match(DataTot$donnee,ETVSp3$donnee)
DataVSp3=subset(DataTot,is.na(test2)==F)
ListDonnee=levels(as.factor(DataVSp3$donnee))
test=match(ETVSp3$donnee,DataVSp3$donnee)
ETVSp4=subset(ETVSp3,is.na(test)==F)

ListDonnee2=levels(as.factor(ETVSp3$donnee))

FauxNeg=vector()

for (i in ListDonnee)
{
  test=subset(DataVSp3,(DataVSp3$donnee==i)&(DataVSp3$espece==SpCible))
  FauxNeg=c(FauxNeg,(nrow(test)==0))
}

Indice=aggregate(ETVSp4$probabilite,by=list(ETVSp4$donnee),FUN=max)
ClassI=as.factor(ceiling(Indice$x*10))
table(FauxNeg,ClassI)
DurSeq=ETVSp4$temps_fin-ETVSp4$temps_debut
IDS=aggregate(DurSeq,by=list(ETVSp4$donnee),FUN=max)

ClassDur=as.factor(ceiling(IDS$x*2))
table(FauxNeg,ClassDur)

#Calcul des faux positifs

ETVSpPos=subset(ETV_filtree,ETV_filtree$espece==SpCible)


DataSp=subset(DataBat,DataBat$espece==SpCible)
#DataSp=subset(DataBat,substr(DataBat$espece,1,3)=="Myo")
#DataSp=DataSp[order(DataSp$frequence),]
#DataSp2=subset(DataSp,(DataSp$frequence>21)&(DataSp$frequence<30))

#write.csv(DataSp2,"DataPleBF.csv",row.names=F)

#DataSp=DataSp1


Heure=sapply(DataSp$donnee,FUN=function(x) substr(x,nchar(x)-9,nchar(x)-8))

NbC=aggregate(DataSp$donnee,by=list(DataSp$participation),FUN=length)
MaxI=aggregate(DataSp$probabilite,by=list(DataSp$participation),FUN=max)


MaxV=merge(ETVSpPos,MaxI,by.x=c("probabilite","participation"),by.y=c("x","Group.1"))

MaxD=merge(DataSp,MaxI,by.x=c("probabilite","participation"),by.y=c("x","Group.1"))

#AValid=sample(MaxD$donne,100)
#write.csv2(AValid,file=paste0("AValid",SpCible,".csv"),row.names=F)

IdDef=vector()
for (i in (1:nrow(MaxV)))
{
  if(MaxV$valid.espece[i]==""){IdDef=c(IdDef,MaxV$obs.espece[i])}else{IdDef=c(IdDef,MaxV$valid.espece[i])}
  
}
FauxPos=(IdDef==SpCible)
FauxPosP=subset(MaxV$participation,FauxPos==F)
VraiPosP=levels(as.factor(ETVSp3$participation))
ListParVerif=as.data.frame(rbind(cbind(FauxPosP,Id=F),cbind(VraiPosP,Id=T)))
LPFMI=merge(ListParVerif,MaxI,by.x="FauxPosP",by.y="Group.1")
LPFMI2=merge(LPFMI,NbC,by.x="FauxPosP",by.y="Group.1")
ClassIP=as.factor(ceiling(LPFMI2$x.x*10))
VP=subset(LPFMI2,LPFMI2$Id==T)
FP=subset(LPFMI2,LPFMI2$Id==F)


plot(VP$x.y,VP$x.x,col=4,log="x",xlab="Nb Contacts",ylab="Probabilite Max")
points(FP$x.y,FP$x.x,col=2)

write.csv(LPFMI2,paste0(SpCible,"_ConfCont.csv"),row.names=F)


#summary(as.factor(DataSp$valid.espece))
#ProtoT=substr(DataSp$donnee,1,7)

Seuil1=0.95
Seuil2=0.8
Seuil3=0



DataSelG=merge(PartSelG,DataSp,by="participation")
#rm(DataSp)

PF=(substr(DataSelG$donnee,1,3)=="Car")
DataPF=subset(DataSelG,(PF)&(DataSelG$probabilite>Seuil2))
Heure=as.numeric(substr(DataPF$donnee,nchar(DataPF$donnee)-9,nchar(DataPF$donnee)-8))

DataSwarm=subset(DataPF,Heure<4)
ActSwarm=aggregate(DataSwarm$donnee,by=list(DataSwarm$participation),FUN=length)
AS2=merge(ActSwarm,PartSelG,by.x="Group.1",by.y="participation")
Act=AS2$x/as.numeric(AS2$DurPF)
AS3=cbind(AS2,Act)
AS4=subset(AS3,(AS3$Act>0)&(AS3$Act<5000))
AS5=subset(AS4,(AS4$Mois=="08")|(AS4$Mois=="09")|(AS4$Mois=="10"))
write.csv(AS5,"AS5.csv",row.names=F)


HMS=lapply(DataPF$donnee,FUN=f2p)

TtSt=vector(length=0)
TtSr=vector(length=0)
TimPos=vector(length=0)
DecMin=vector(length=0)
for (i in 1:nrow(DataPF))
{
  Date1=HMS[[i]][1]
  H1=as.numeric(format(Date1, "%H")) +    as.numeric(format(Date1, "%M"))/60
  Date2=format(as.Date(Date1-43200*(H1<12)),format="%Y/%m/%d")
  Date3=format(as.Date(Date1+43200*(H1>12)),format="%Y/%m/%d")
  St=sunrise.set(DataPF$latitude[i],DataPF$longitude[i],Date2,timezone="CET")[1,2]
  Sr=sunrise.set(DataPF$latitude[i],DataPF$longitude[i],Date3,timezone="CET")[1,1]
  TtSt[i]=difftime(Date1,St,units="mins")
  TtSr[i]=difftime(Sr,Date1,units="mins")
  TimPos[i]=TtSt[i]/as.numeric(difftime(Sr,St,units="mins"))
  DecMin[i]=min(as.numeric(TtSt[i]),as.numeric(TtSr[i]))
print(paste(i,"/",nrow(DataPF)))
  }

DataDM=cbind(DataPF,TtSt,TtSr,DecMin)
write.csv(DataDM,"DataDM.csv",row.names=F)
test=(DecMin<0)
test2=subset(DataPF,test)
#test3=which(DataPF$donnee=="Car340575-2014-Pass1-Z1-Croix_de_Pelisse0_20140321_230904_461")

#DataPF=cbind(DataPF,DecMin)
DMM=aggregate(DecMin,by=list(DataPF$participation),FUN=min)
DMMC=merge(DMM,NbC,by="Group.1")
plot(DMMC$x.x,DMMC$x.y,log="y")

CoordM=aggregate(cbind(DataPF$longitude,DataPF$latitude,DataPF$Mois),by=list(DataPF$participation),FUN=mean)

DataMap=merge(DMMC,CoordM,by="Group.1")
colnames(DataMap)=c("part","DecMin","NbC","x","y","Mois")

write.csv(DataMap,paste0("DataPF",SpCible,".csv"),row.names=F)
write.csv(DataMap,"DataPFDecMin.csv",row.names=F)


PresVerif=subset(PartSelG,is.na(match(PartSelG$participation,VraiPosP))==F)
write.csv(PresVerif,"PresVerif.csv",row.names=F)

PresTP=subset(PartSelG
    ,(is.na(match(PartSelG$participation,subset(MaxI$Group.1,MaxI$x>Seuil1)))==F)
    &(is.na(match(PartSelG$participation,FauxPosP))==T))
write.csv(PresTP,"PresTP.csv",row.names=F)

PresP=subset(PartSelG
             ,(is.na(match(PartSelG$participation,subset(MaxI$Group.1,MaxI$x>Seuil2)))==F)
             &(is.na(match(PartSelG$participation,FauxPosP))==T))
write.csv(PresP,"PresP.csv",row.names=F)

PresD=subset(PartSelG,
             (is.na(match(PartSelG$participation,subset(MaxI$Group.1,MaxI$x>Seuil3)))==F)
             &(is.na(match(PartSelG$participation,FauxPosP))==T))
write.csv(PresD,"PresD.csv",row.names=F)

AbVerif=subset(PartSelG,is.na(match(PartSelG$participation,FauxPosP))==F)
write.csv(AbVerif,"AbVerif.csv",row.names=F)

AbP=subset(PartSelG,(is.na(match(PartSelG$participation,MaxI$Group.1))==T)&(PartSelG$nb_tc>0))
AbP2=subset(PartSelG,is.na(match(PartSelG$participation,subset(MaxI$Group.1,MaxI$x<Seuil3)))==F)
AbP=rbind(AbP,AbP2)
write.csv(AbP,"AbP.csv",row.names=F)


test3=((PresVerif$V2>43.71)&(PresVerif$V1>3.70)&(PresVerif$V2<43.87)&(PresVerif$V1<3.98))
test4=((PresTP$V2>43.71)&(PresTP$V1>3.70)&(PresTP$V2<43.87)&(PresTP$V1<3.98))
test5=((PresP$V2>43.71)&(PresP$V1>3.70)&(PresP$V2<43.87)&(PresP$V1<3.98))





###########################

SpCible1="Myodau"

Data1=fread(paste0(SpCible1,"_ConfCont.csv"))

SpCible2="Myocap"

Data2=fread(paste0(SpCible2,"_ConfCont.csv"))

Data12=merge(Data1,Data2,by="FauxPosP")
Ratio=Data12$x.y.y/(Data12$x.y.x+Data12$x.y.y)
Data12=cbind(Data12,Ratio)

VP=subset(Data12,Data12$Id.y==T)
FP=subset(Data12,Data12$Id.y==F)

plot(VP$Ratio,VP$x.x.y,col=4,xlab="Ratio Contacts",ylab="Probabilite Max")
points(FP$Ratio,FP$x.x.y,col=2)



#DataSp2=subset(DataSp,DataSp$probabilite>0.5)
#DataSp2=subset(DataSp2,is.na(match(DataSp2$participation,PresP$participation))==F)


gc()


#variable locs

#test=nchar(DataSelG$donnee)

#test1=subset(DataSelG,nchar(DataSelG$donnee)<43)
#test2=subset(DataSelG,nchar(DataSelG$donnee)>75)

#test1b=substr(test1$donnee,1,27)
#test2b=substr(test2$donnee,1,27)

LocaPartData=as.factor(substr(DataSelG$donnee,1,27))
SitPartData=as.factor(substr(DataSelG$donnee,1,21))

#micro=as.factor(substr(DataSelG$donnee,nchar(DataSelG$donnee)-11,nchar(DataSelG$donnee)-10))
micro=as.factor(substr(DataSelG$donnee,nchar(DataSelG$donnee)-21,nchar(DataSelG$donnee)-20))




#erreurs=subset(DataSelG,as.numeric(as.character(micro))>1)

microV=(micro=="_1")

Det=substr(DataSelG$detecteur_enregistreur_type,1,3)
#Det[Det==""]="D24"

Prot=substr(DataSelG$site,1,20)

#Date1=as.factor(substr(DataSelG$donnee,nchar(DataSelG$donnee)-18,nchar(DataSelG$donnee)-11))
Date1=as.POSIXlt(f2p(DataSelG$donnee))
Nuit=format(as.Date(Date1-43200*(Date1$hour<12)),format="%d/%m/%Y")
Nuit[is.na(Nuit)]=0


DataSel2=cbind(DataSelG,LocaPartData,SitPartData,micro,microV,Det,Prot,Date1,Nuit)

  Dataaggr=aggregate(DataSel2$espece,by=c(list(DataSel2$Prot),list(DataSel2$participation),list(DataSel2$Nuit))
                     ,FUN=function(x) length(x))
 
DataaggrMoy=aggregate(Dataaggr$x,by=c(list(Dataaggr$Group.1),list(Dataaggr$Group.2)),FUN=mean)

DataaggrMerge=merge(DataaggrMoy,PartSelG,by.x="Group.2",by.y="participation")
#write.csv(DataaggrMerge,"DataaggrM_Myodau.csv",row.names=F)

SeuilTF=c(11,92,264)
SeuilF=c(3,10,6)
SeuilM=c(1,2,1)

summary(as.factor(DataaggrMerge$Group.1))

ActTF1=subset(DataaggrMerge,(DataaggrMerge$x>SeuilTF[1])&(DataaggrMerge$Group.1=="Vigie-chiro - Routie"))
ActTF2=subset(DataaggrMerge,(DataaggrMerge$x>SeuilTF[2])&(DataaggrMerge$Group.1=="Vigiechiro - Pédestr"))
ActTF3=subset(DataaggrMerge,(DataaggrMerge$x>SeuilTF[3])&(DataaggrMerge$Group.1=="Vigiechiro - Point F"))
ActTF=rbind(ActTF1,ActTF2,ActTF3)
write.csv(ActTF,"ActTF.csv",row.names=F)

ActF1=subset(DataaggrMerge,(DataaggrMerge$x>SeuilF[1])&(DataaggrMerge$Group.1=="Vigie-chiro - Routie"))
ActF2=subset(DataaggrMerge,(DataaggrMerge$x>SeuilF[2])&(DataaggrMerge$Group.1=="Vigiechiro - Pédestr"))
ActF3=subset(DataaggrMerge,(DataaggrMerge$x>SeuilF[3])&(DataaggrMerge$Group.1=="Vigiechiro - Point F"))
ActF=rbind(ActF1,ActF2,ActF3)
write.csv(ActF,"ActF.csv",row.names=F)

ActM1=subset(DataaggrMerge,(DataaggrMerge$x>SeuilM[1])&(DataaggrMerge$Group.1=="Vigie-chiro - Routie"))
ActM2=subset(DataaggrMerge,(DataaggrMerge$x>SeuilM[2])&(DataaggrMerge$Group.1=="Vigiechiro - Pédestr"))
ActM3=subset(DataaggrMerge,(DataaggrMerge$x>SeuilM[3])&(DataaggrMerge$Group.1=="Vigiechiro - Point F"))
ActM=rbind(ActM1,ActM2,ActM3)
write.csv(ActM,"ActM.csv",row.names=F)

write.csv(DataaggrMerge,"ActFaible.csv",row.names=F)


