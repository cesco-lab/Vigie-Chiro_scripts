library(data.table)
library(raster)
library(beepr)
library(plyr)

GroupMyo=F
GroupPle=F
GLM=F
#DataRP=fread("C:/Users/yvesb/Documents/GitHub/VigieChiro/data/data_vigieChiro_DataRP_SpTron_50_site_55sp_withAbs.csv")
DataRP=fread("C:/Users/yvesb/Downloads/DataRP_SpTron_woS_0.csv")
DataPF=fread("C:/Users/yvesb/Documents/www/SpNuit_2DI_0_DataLP_PF_exportTot.csv")
TagData=c("50")
SpeciesList=fread("C:/Users/yvesb/Documents/Tadarida/Tadarida-C/tadaridac_src/other_inputs/SpeciesList.csv")
Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv",encoding="UTF-8")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
#AnomWeather=fread("./VigieChiro/Weather/SLAll_W.csv")
AnomWeather=NA
YearsSelected=c(2006:2021)
#Bioclim=fread("./www/GI_sites_localites.csv")
#ClassHab=fread("./vrac_md_dell2021/Classes_coord.csv")
#Hab="All" #"No selection" if no filter "All" if running all habitats, name of the habitat otherwise
#Hab="Urbain"
#Hab="No selection"
PurgeZeros=F#
FmaxRP=fread("C:/Users/yvesb/Downloads/FmaxRP.csv")
OutDir="C:/Users/yvesb/Documents/VigieChiro/Raw/forGLM2203/"
DecProb=fread("C:/Users/yvesb/Downloads/DecProb.csv")

Dec2=subset(DecProb,DecProb$x==2)

DataPF=subset(DataPF,DataPF$participation %in% Dec2$Group.1)


dir.create(OutDir)
  TagSave=TagData

#filter out unappropriate sample rates
DataRP=merge(DataRP,FmaxRP,by.x=c("participation","num_micro"),by.y=c("Group.1","Group.2"))
boxplot(subset(DataRP,DataRP$espece=="Pippip")$x)
boxplot(subset(DataRP,DataRP$espece=="Nycnoc")$x)


DataRP=subset(DataRP,DataRP$temps_enr>150)
DataRP=subset(DataRP,DataRP$temps_enr<800)



DataPF$x=192



if(!("nb_contacts" %in% names(DataRP)))
{
  DataRP$nb_contacts=DataRP$nb_contacts_strict
  DataRP=subset(DataRP,!is.na(DataRP$nb_contacts))
}
if(GroupPle){DataRP$espece[substr(DataRP$espece,1,3)=="Ple"]="Plespp"}
if(GroupMyo){DataRP$espece[substr(DataRP$espece,1,3)=="Myo"]="Myospp"}
SumEsp=aggregate(DataRP$nb_contacts,by=list(DataRP$espece),FUN=sum)
SpeciesOrder=SumEsp$Group.1[order(SumEsp$x,decreasing=T)]

DataPF$year=as.numeric(substr(DataPF$Nuit,1,4))
table(DataPF$year)
DataPF=subset(DataPF,DataPF$year %in% YearsSelected)

DataPF$nb_contacts=DataPF$nb_contacts_nd

SampleUnique=unique(subset(DataPF,select=c("participation","Nuit","num_micro","year")))

ParticipPF=subset(Particip,Particip$point!="")

SampleUniqueP=merge(SampleUnique,ParticipPF,by="participation")

PointAgg=aggregate(SampleUniqueP$participation
                   ,by=list(SampleUniqueP$site,SampleUniqueP$point
                            ,SampleUniqueP$year)
                   ,FUN=length)

PointAggY=aggregate(PointAgg$Group.3
                    ,by=list(PointAgg$Group.1,PointAgg$Group.2)
                    ,FUN=length)
table(PointAggY$x) #number of years per points

PointsRepetes=subset(PointAggY,PointAggY$x>1)
Dep=substr(PointsRepetes$Group.1,25,26)
table(Dep)
ParticipRepetes=subset(ParticipPF,paste(ParticipPF$site,ParticipPF$point) %in%
                         paste(PointsRepetes$Group.1,PointsRepetes$Group.2))


DataPFP=merge(DataPF,Particip,by="participation")

Dep=substr(DataPFP$site,25,26)
table(Dep)



DataPFP$protocole=tstrsplit(DataPFP$site,split="-")[[2]]

DataPFP$mat=substr(DataPFP$detecteur_enregistreur_type,1,4) 
table(DataPFP$mat)
DataPFP$micro0_type[DataPFP$micro0_type=="SMX-U1"]="SMM-U1"
DataPFP$micro0_type[DataPFP$micro0_type=="Micro externe sans cornet"]=""
DataPFP$micro0_type[DataPFP$micro0_type=="Autre micro externe"]=""
DataPFP$micro0_type[DataPFP$micro0_type=="Micro interne"]=""
DataPFP$micro0_type[DataPFP$micro0_type=="SMX-U1"]="SMM-U1"
DataPFP$micro0_type[!(DataPFP$detecteur_enregistreur_type %in%
                       c("SM2BAT","SM2BAT+","SM4"))]=""

DataPFP$mat=paste(DataPFP$mat,substr(DataPFP$micro0_type,1,6))

MatSum=aggregate(DataPFP$participation,by=list(DataPFP$mat), FUN=length)

MatSum=MatSum[order(MatSum$x,decreasing=T),]
MatSum$csx=cumsum(MatSum$x)
MatSum$csx=c(0,MatSum$csx[1:(length(MatSum$csx)-1)])
CommonMat=subset(MatSum$Group.1,MatSum$csx<0.99*max(MatSum$csx))
DiscardMat=subset(MatSum$Group.1,MatSum$csx>=0.99*max(MatSum$csx))
print(nrow(DataPFP))
DataPFP=subset(DataPFP,DataPFP$mat %in% CommonMat)
print(nrow(DataPFP))

DataPFP$sample_cat="point_fixe"
DataPFP$expansion_direct="direct"
DataPFP$temps_enr=360
DataPFP$SampleRate=384000 #to be modified using SR values from tc but useless for now


DataPFrepetes=subset(DataPFP,DataPFP$participation %in% ParticipRepetes$participation)
DataPFrepetes$siteloc=paste(DataPFrepetes$site,DataPFrepetes$point)

Dep=substr(DataPFrepetes$site,25,26)
table(Dep)


SampleUniqueRep=unique(subset(DataPFrepetes,select=c("participation","Nuit","num_micro","year","x","mat"
                                                     ,"temps_enr","protocole","site","point","siteloc"
                                                     ,"expansion_direct")))
table(SampleUniqueRep$year)
table(SampleUniqueRep$expansion_direct)


#add participaton and site info in DataRP
DataRP=merge(DataRP,Particip,by="participation")
SiteLoc$Tron=ifelse(nchar(SiteLoc$nom)>2,tstrsplit(SiteLoc$nom,split=" ")[[2]],SiteLoc$nom)
SiteLoc$Sector=ifelse(nchar(SiteLoc$nom)>2,tstrsplit(SiteLoc$nom,split=" ")[[3]],3)
SiteLocRP=subset(SiteLoc,SiteLoc$Sector==3)
table(SiteLocRP$Tron)
DataRP$Tron=as.character(DataRP$Tron)
DataRP=merge(DataRP,SiteLocRP,by=c("site","Tron"))

DataRP$protocole=tstrsplit(DataRP$site,split="-")[[2]]
DataRP$protocole[DataRP$protocole=="chiro "]="Routier"
table(DataRP$protocole)
#table(substr(DataRP$site,1,15))

DataRP$expansion_direct=ifelse(DataRP$num_micro,ifelse(DataRP$canal_enregistrement_direct=="DROITE"
                                                       ,"direct"
                                                       ,ifelse(DataRP$canal_expansion_temps=="DROITE"
                                                               ,"expansion"
                                                               ,"NA"))
                               ,ifelse(DataRP$canal_enregistrement_direct=="GAUCHE"
                                       ,"direct"
                                       ,ifelse(DataRP$canal_expansion_temps=="GAUCHE"
                                               ,"expansion"
                                               ,"NA")))
table(DataRP$expansion_direct)



DataRP$year=substr(DataRP$date_debut,7,10)
table(DataRP$year)
fwrite(DataRP,"DataRP.csv",sep=";")


DataRP$mat=substr(DataRP$detecteur_enregistreur_type,1,4) 
table(DataRP$mat)
DataRP$micro0_type[DataRP$micro0_type=="SMX-U1"]="SMM-U1"
DataRP$micro0_type[DataRP$micro0_type=="Micro externe sans cornet"]=""
DataRP$micro0_type[DataRP$micro0_type=="Autre micro externe"]=""
DataRP$micro0_type[DataRP$micro0_type=="Micro interne"]=""
DataRP$micro0_type[DataRP$micro0_type=="SMX-U1"]="SMM-U1"
DataRP$micro0_type[!(DataRP$detecteur_enregistreur_type %in%
                       c("SM2BAT","SM2BAT+","SM4"))]=""

DataRP$mat=paste(DataRP$mat,substr(DataRP$micro0_type,1,6))

MatSum=aggregate(DataRP$participation,by=list(DataRP$mat), FUN=length)

MatSum=MatSum[order(MatSum$x,decreasing=T),]
MatSum$csx=cumsum(MatSum$x)
MatSum$csx=c(0,MatSum$csx[1:(length(MatSum$csx)-1)])
CommonMat=subset(MatSum$Group.1,MatSum$csx<0.99*max(MatSum$csx))
DiscardMat=subset(MatSum$Group.1,MatSum$csx>=0.99*max(MatSum$csx))
print(nrow(DataRP))
DataRP=subset(DataRP,DataRP$mat %in% CommonMat)
print(nrow(DataRP))

DataRP$julian=yday(as.Date(DataRP$date_debut))
summary(DataRP$julian)
DataRP$siteloc=paste(DataRP$site,DataRP$Tron)

#SampleUniqueRP=unique(subset(DataRP,select=c("participation","site","Tron","siteloc","num_micro","year","x"
 #                                            ,"protocole","mat"
  #                                           ,"temps_enr","julian","expansion_direct")))
SampleUniqueRP=unique(DataRP,by=c("participation","site","Tron","siteloc","num_micro","year","x"
                                                                              ,"protocole","mat"
                                                                             ,"temps_enr","julian","expansion_direct"))
SampleUniqueRP$espece=NULL
SampleUniqueRP$nb_contacts=NULL
SampleUniqueRP$score_max=NULL



#SpeciesShort=subset(SpeciesList,SpeciesList$Group==GroupSel)
if(GroupMyo){
  DataRP$espece[substr(DataRP$espece,1,3)=="Myo"]="Myospp"
  DataPFrepetes$espece[substr(DataPFrepetes$espece,1,3)=="Myo"]="Myospp"
}

if(GroupPle)
{
  DataRP$espece[substr(DataRP$espece,1,3)=="Ple"]="Plespp"
  DataPFrepetes$espece[substr(DataPFrepetes$espece,1,3)=="Ple"]="Plespp"
  
  }
DataSp=rbind(subset(DataRP,select=c("espece","nb_contacts"))
                       ,subset(DataPFrepetes,select=c("espece","nb_contacts")))
             
NDataSp=aggregate(DataSp$nb_contacts,by=list(DataSp$espece),FUN=sum)
NDataSp=NDataSp[order(NDataSp$x,decreasing = T),]
colnames(NDataSp)=c("species","weight")
fwrite(NDataSp,"NDataSp.csv")


DataSpPos=subset(DataSp,DataSp$nb_contacts>0)
AbDataSp=aggregate(DataSpPos$nb_contacts,by=list(DataSpPos$espece)
                   ,FUN=sum)
names(AbDataSp)[ncol(AbDataSp)]="NbContactsTot"

#OccDataSp=aggregate(DataSpPos$nb_contacts,by=list(DataSpPos$espece,DataSpPos$protocole)
#                    ,FUN=length)
#AbDataSp$NbOccurrences=OccDataSp$x
#head(AbDataSp)
#fwrite(AbDataSp,"GrandsTotauxSpProtocoles.csv",sep=";")

#SpeciesOrder=subset(SpeciesOrder,grepl("Myo",SpeciesOrder))

SpPZ=vector()
PZ=vector()
for (i in 1:length(SpeciesOrder))
  #for (i in 1:3)
{
  print(paste(SpeciesOrder[i]))
  DataRPi=subset(DataRP,DataRP$espece==SpeciesOrder[i])
  
  #DataRPi=merge(DataRPi,AnomWeather,by=c("participation"))
  
  DataPFi=subset(DataPFrepetes,DataPFrepetes$espece==SpeciesOrder[i])
  
  table(DataPFi$year)
  DataPFi_w0=merge(DataPFi,SampleUniqueRep
                   ,by=c("participation","Nuit","num_micro","year","x","mat"
                   ,"temps_enr","protocole","site","point","siteloc","expansion_direct")
                   ,all.y=T)
  
  Dep=substr(DataPFi_w0$site,25,26)
  table(Dep)
  
  
  ndata=nrow(DataPFi_w0)
  DataPFi_w0$protocole=NULL
  
  DataPFi_w0=merge(DataPFi_w0,SiteLoc,by.x=c("site","point")
                   ,by.y=c("site","nom"))
  
  Dep=substr(DataPFi_w0$site,25,26)
  table(Dep)
  
  
  if(nrow(DataPFi_w0)<ndata) 
  {
    print(paste(ndata-nrow(DataPFi_w0)
                ,"données perdues car absentes de la table localites"))
    ndata=nrow(DataPFi_w0)
  }
  
  backup=DataPFi_w0
  #DataPFi_w0$protocole=NULL
  #DataPFi_w0=merge(DataPFi_w0,Bioclim,by=c("longitude","latitude"))
  #,by.y=c("Group.1","Group.2"))
  
  #DataPFi_w0=merge(DataPFi_w0,AnomWeather,by=c("participation","Nuit"))
  #if(nrow(DataPFi_w0)<ndata) 
  #{
  #print(paste(ndata-nrow(DataPFi_w0)
  #           ,"données perdues car absentes de la table anomalies"))
  # ndata=nrow(DataPFi_w0)
  #}
  
  
  
  DataPFi_w0$nb_contacts[is.na(DataPFi_w0$nb_contacts)]=0
  DataPFi_w0$espece=SpeciesOrder[i]
  
  DataPFi_w0$month=as.numeric(substr(DataPFi_w0$Nuit,6,7))
  sum(is.na(DataPFi_w0$month))
  #barplot(table(DataPFi_w0$month))
  #boxplot(DataPFi_w0$nb_contacts~DataPFi_w0$month)
  DataPFi_w0$julian <- yday(DataPFi_w0$Nuit)
  #barplot(table(DataPFi_w0$julian))
  #hist(DataPFi_w0$julian)
  DataPFi_w0$num_site=as.numeric(gsub("Vigiechiro - Point Fixe-",""
                                      ,DataPFi_w0$site))
  DataPFi_w0$nb_Tron_strict=10
  #fwrite(DataPFi_w0,paste0("./VigieChiro/Trends/DataPF/DataPFi_w0_"
  #                        ,SpeciesOrder[i],".csv"),sep=";")
  
  #add localities variable
  DataRPi$siteloc=paste(DataRPi$num_site,DataRPi$Tron)
  DataPFi_w0$siteloc=paste(DataPFi_w0$num_site,DataPFi_w0$point)
  
  Dep=substr(DataPFi_w0$site,25,26)
  table(Dep)
  
  
  
  #ADD ZEROS on DataRP
  print(nrow(DataRPi))
  print(table(DataRPi$espece))
  
  DataRPi_save=DataRPi
  DataRPi=DataRPi_save
  test=match(paste(DataRPi$participation,DataRPi$site,DataRPi$Tron,DataRPi$num_micro)
             ,paste(SampleUniqueRP$participation,SampleUniqueRP$site,SampleUniqueRP$Tron,SampleUniqueRP$num_micro))
  table(is.na(test))
  DataRPi=subset(DataRPi,select=c("participation","site","Tron","num_micro","year","x"
                                  ,"protocole","mat"
                                  ,"temps_enr","julian","expansion_direct","espece","nb_contacts","score_max")
  )
  DataRPi=join(DataRPi,SampleUniqueRP
                ,by=c("participation","site","Tron","num_micro","year","x"
                      ,"protocole","mat"#,"siteloc"
                      ,"temps_enr","julian","expansion_direct"
                     )
                ,type="right")
  
  print(table(DataRPi$espece))
  DataRPi$espece=SpeciesOrder[i]
  print(summary(is.na(DataRPi$nb_contacts)))
  DataRPi$nb_contacts[is.na(DataRPi$nb_contacts)]=0
  
  print(nrow(DataRPi))    
  
  
  
  DataRPi_purge=subset(DataRPi
                       ,select=(colnames(DataRPi) %in% colnames(DataPFi_w0)))
  
  
  
  DataPFi_purge=subset(DataPFi_w0
                       ,select=(colnames(DataPFi_w0) %in% colnames(DataRPi_purge)))
  
  
  Dep=substr(DataPFi_purge$site,25,26)
  table(Dep)
  
  
  table(names(DataPFi_purge))
  dim(DataPFi_purge)
  
  DataRPPF=rbindlist(list(DataRPi_purge,DataPFi_purge),use.names=T)
  #for test
  #DataRPPF=DataRPi_purge
  #DataRPPF=DataPFi_purge
  
  
  
  DataRPPF$nb_contacts_strict=DataRPPF$nb_contacts
  
  if(!is.na(AnomWeather)){
    DataRPPF$AnomClimateNight=apply(cbind(DataRPPF$TN_1_1,DataRPPF$TX_0_0)
                                    ,MARGIN=1,function(x) mean(x,na.rm=T))
    
    DataRPPF$AnomTemperature=mapply(function(x,y) ifelse(is.na(x),y,x)
                                    ,DataRPPF$AnomClimateNight
                                    ,DataRPPF$NTempM_NCEP_0_0
    )
    DataRPPF$AnomTemperature[is.na(DataRPPF$AnomTemperature)]=0
    DataRPPF$NWM_NCEP_0_0[is.na(DataRPPF$NWM_NCEP_0_0)]=0
  }
  if(i==1)
  {
    ValidCir=aggregate(DataRPPF$participation
                       ,by=c(list(DataRPPF$site),
                             list(DataRPPF$date_debut)
                             ,list(DataRPPF$expansion_direct)
                             ,list(DataRPPF$siteloc))
                       ,FUN=length)
    fwrite(ValidCir,"ValidCir.csv",sep=";")
  }
  
  
  
  if(PurgeZeros)
  {
    PropZeros=aggregate(DataRPPF$nb_contacts_strict,by=list(DataRPPF$siteloc)
                        ,FUN=function(x) (sum(x==0))/length(x))
    PZ_threshold=mean(PropZeros$x)+(1-mean(PropZeros$x))*0.9
    SL_PZ=subset(PropZeros$Group.1,PropZeros$x<PZ_threshold)
    #Excl_PZ=subset(PropZeros$Group.1,PropZeros$x>=PZ_threshold)
    DataRPPF=subset(DataRPPF,DataRPPF$siteloc %in% SL_PZ)
    SpPZ=c(SpPZ,SpeciesOrder[i])
    PZ=c(PZ,PZ_threshold)
    
  }
  
  
  
  fwrite(DataRPPF,paste0(OutDir,"/DataRPPF_",SpeciesOrder[i],".csv"))
  #beep()
}

