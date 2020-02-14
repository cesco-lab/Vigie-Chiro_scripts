library(data.table)

source("GitHub/VigieChiro/main_glm.r")
source("./GitHub/VigieChiro/script_trend.r")

#DataRP=fread("C:/Users/Yves Bas/Documents/GitHub/VigieChiro/data/data_vigieChiro_DataRP_SpTron_50_site_55sp_withAbs.csv")
DataRP=fread("C:/Users/Yves Bas/Documents/GitHub/VigieChiro/data/data_vigieChiro_DataRP_SpTron_90_TronPoint_55sp_withAbs.csv")
DataPF=fread("C:/wamp64/www/SpNuit2_90_DataLP_PF_exportTot.csv")
TagData=c("90TronPF_wAnom_woint")
VarEffectI=c("year","poly(julian,2)","sample_cat"
             #,"nb_Tron_strict"
             ,"temps_enr"
             ,"mat"
             ,"SpBioC1","SpBioC12"
             ,"expansion_direct"
             ,"AnomTemperature","NWM_NCEP_0_0"
)
SpeciesList=fread("SpeciesList.csv")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
AnomWeather=fread("./VigieChiro/Weather/SLAll_W.csv")
YearsSelected=c(2006:2020)
Bioclim=fread("./VigieChiro/GIS/GI_coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv")
RandomEffectI="(1|siteloc)"

#filter out unappropriate sample rates
DataRP=subset(DataRP,DataRP$SampleRate<=DataRP$seuilSR_sup)
DataRP=subset(DataRP,DataRP$SampleRate>=DataRP$seuilSR_inf)


if(!("nb_contacts" %in% names(DataRP)))
{
  DataRP$nb_contacts=DataRP$nb_contacts_strict
  DataRP=subset(DataRP,!is.na(DataRP$nb_contacts))
}
SumEsp=aggregate(DataRP$nb_contacts,by=list(DataRP$espece),FUN=sum)
SpeciesOrder=SumEsp$Group.1[order(SumEsp$x,decreasing=T)]

DataPF$year=as.numeric(substr(DataPF$Nuit,1,4))
table(DataPF$year)
DataPF=subset(DataPF,DataPF$year %in% YearsSelected)

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

DataPFrepetes=subset(DataPF,DataPF$participation %in% ParticipRepetes$participation)

SampleUniqueRep=unique(subset(DataPFrepetes,select=c("participation","Nuit","num_micro","year")))
table(SampleUniqueRep$year)

#SpeciesShort=subset(SpeciesList,SpeciesList$Group==GroupSel)
DataSp=rbind(subset(DataRP,select=c("espece","nb_contacts"))
             ,subset(DataPFrepetes,select=c("espece","nb_contacts")))
DataSp$espece[substr(DataSp$espece,1,3)=="Myo"]="Myospp"
NDataSp=aggregate(DataSp$nb_contacts,by=list(DataSp$espece),FUN=sum)
NDataSp=NDataSp[order(NDataSp$x,decreasing = T),]
colnames(NDataSp)=c("species","weight")
fwrite(NDataSp,"NDataSp.csv")


for (i in 1:length(SpeciesOrder))
#for (i in 1:3)
{
  DataRPi=subset(DataRP,DataRP$espece==SpeciesOrder[i])
  
  DataRPi=merge(DataRPi,AnomWeather,by=c("participation"))
  
  DataPFi=subset(DataPFrepetes,DataPFrepetes$espece==SpeciesOrder[i])
  
  table(DataPFi$year)
  DataPFi_w0=merge(DataPFi,SampleUniqueRep
                   ,by=c("participation","Nuit","num_micro","year"),all.y=T)
  ndata=nrow(DataPFi_w0)
  
  DataPFi_w0=merge(DataPFi_w0,Particip,by="participation")
  if(nrow(DataPFi_w0)<ndata) 
  {
    print(paste(ndata-nrow(DataPFi_w0)
                ,"données perdues car absentes de la table participation"))
    ndata=nrow(DataPFi_w0)
  }
  DataPFi_w0=merge(DataPFi_w0,SiteLoc,by.x=c("site","point")
                   ,by.y=c("site","nom"))
  if(nrow(DataPFi_w0)<ndata) 
  {
    print(paste(ndata-nrow(DataPFi_w0)
                ,"données perdues car absentes de la table localites"))
    ndata=nrow(DataPFi_w0)
  }
  
  backup=DataPFi_w0
  DataPFi_w0=merge(DataPFi_w0,Bioclim,by.x=c("longitude","latitude")
                   ,by.y=c("Group.1","Group.2"))
  if(nrow(DataPFi_w0)<ndata) 
  {
    print(paste(ndata-nrow(DataPFi_w0)
                ,"données perdues car absentes de la table bioclim"))
    Tot=merge(backup,Bioclim,by.x=c("longitude","latitude")
              ,by.y=c("Group.1","Group.2"),all.x=T)
    test2=Tot$SpBioC1
    test3=is.na(test2)
    test=subset(Tot,test3)
    plot(test$longitude,test$latitude)
    print("Exemple :")
    Exemple=test[sample.int(nrow(test),1),1:2]
    print(paste0(Exemple[1,2],",",Exemple[1,1]))
    ndata=nrow(DataPFi_w0)
  }
  
  DataPFi_w0=merge(DataPFi_w0,AnomWeather,by=c("participation","Nuit"))
  if(nrow(DataPFi_w0)<ndata) 
  {
    print(paste(ndata-nrow(DataPFi_w0)
                ,"données perdues car absentes de la table anomalies"))
    ndata=nrow(DataPFi_w0)
  }
  
  
  
  DataPFi_w0$nb_contacts[is.na(DataPFi_w0$nb_contacts)]=0
  DataPFi_w0$espece=SpeciesOrder[i]
  
  DataPFi_w0$month=as.numeric(substr(DataPFi_w0$Nuit,6,7))
  sum(is.na(DataPFi_w0$month))
  barplot(table(DataPFi_w0$month))
  boxplot(DataPFi_w0$nb_contacts~DataPFi_w0$month)
  DataPFi_w0$julian <- yday(DataPFi_w0$Nuit)
  barplot(table(DataPFi_w0$julian))
  hist(DataPFi_w0$julian)
  DataPFi_w0$sample_cat="point_fixe"
  DataPFi_w0$expansion_direct="direct"
  DataPFi_w0$temps_enr=360
  DataPFi_w0$SampleRate=384000 #to be modified using SR values from tc but useless for now
  DataPFi_w0$num_site=as.numeric(gsub("Vigiechiro - Point Fixe-",""
                                      ,DataPFi_w0$site))
  DataPFi_w0$nb_Tron_strict=10
  #fwrite(DataPFi_w0,paste0("./VigieChiro/Trends/DataPF/DataPFi_w0_"
  #                        ,SpeciesOrder[i],".csv"),sep=";")
  
  #add localities variable
  DataRPi$siteloc=paste(DataRPi$num_site,DataRPi$Tron)
  DataPFi_w0$siteloc=paste(DataPFi_w0$num_site,DataPFi_w0$point)
  
  
  DataRPi_purge=subset(DataRPi
                       ,select=(colnames(DataRPi) %in% colnames(DataPFi_w0)))
  DataPFi_purge=subset(DataPFi_w0
                       ,select=(colnames(DataPFi_w0) %in% colnames(DataRPi_purge)))
  
  DataRPPF=rbindlist(list(DataRPi_purge,DataPFi_purge),use.names=T)
  #for test
  #DataRPPF=DataRPi_purge
  #DataRPPF=DataPFi_purge
  
  
  DataRPPF$mat=substr(DataRPPF$detecteur_enregistreur_type,1,4) 
  DataRPPF$micro0_type[DataRPPF$micro0_type=="SMX-U1"]="SMM-U1"
  DataRPPF$micro0_type[DataRPPF$micro0_type=="Micro externe sans cornet"]=""
  DataRPPF$micro0_type[DataRPPF$micro0_type=="Autre micro externe"]=""
  DataRPPF$micro0_type[DataRPPF$micro0_type=="Micro interne"]=""
  DataRPPF$micro0_type[DataRPPF$micro0_type=="SMX-U1"]="SMM-U1"
  DataRPPF$micro0_type[!(DataRPPF$detecteur_enregistreur_type %in%
                           c("SM2BAT","SM2BAT+","SM4"))]=""
  
  DataRPPF$mat=paste(DataRPPF$mat,substr(DataRPPF$micro0_type,1,6))
  
  MatSum=aggregate(DataRPPF$participation,by=list(DataRPPF$mat), FUN=length)
  
  MatSum=MatSum[order(MatSum$x,decreasing=T),]
  MatSum$csx=cumsum(MatSum$x)
  CommonMat=subset(MatSum$Group.1,MatSum$csx<0.95*max(MatSum$csx))
  
  DataRPPF=subset(DataRPPF,DataRPPF$mat %in% CommonMat)
  
  DataRPPF$nb_contacts_strict=DataRPPF$nb_contacts
  
  DataRPPF$AnomClimateNight=apply(cbind(DataRPPF$TN_1_1,DataRPPF$TX_0_0)
                                  ,MARGIN=1,function(x) mean(x,na.rm=T))
  
  DataRPPF$AnomTemperature=mapply(function(x,y) ifelse(is.na(x),y,x)
                                  ,DataRPPF$AnomClimateNight
                                  ,DataRPPF$NTempM_NCEP_0_0
  )
  DataRPPF$AnomTemperature[is.na(DataRPPF$AnomTemperature)]=0
  DataRPPF$NWM_NCEP_0_0[is.na(DataRPPF$NWM_NCEP_0_0)]=0
  
  #filtering locations makes models unstable
  #NdataPerSite=aggregate(DataRPPF$nb_contacts,by=list(DataRPPF$siteloc)
   #                      ,FUN=sum)
  #SitePresence=subset(NdataPerSite$Group.1,NdataPerSite$x>0)
  #DataRPPF=subset(DataRPPF,DataRPPF$siteloc %in% SitePresence)
  
  main.glm(id=paste0(Sys.Date(),SpeciesOrder[i])
           ,
           donneesAll=list(DataRPPF)
           ,
           donneesName=TagData
           ,
           method="glmmTMB"
           ,
           family="nbinom2"
           ,
           #only_direct=""
           only_direct=
           ,
           only_exp=""
           ,
           seuilOccu=2
           ,
           col_sp="espece"
           ,
           col_date_julien="julian"
           ,
           col_site="site"
           ,
           col_nbcontact="nb_contacts"
           ,
           assessIC= TRUE
           ,
           listSp=NULL
           ,
           tabsp="SpeciesList.csv"
           
           ,
           first_year=NULL
           ,
           last_year=NULL
           ,
           figure=TRUE
           ,
           description=c("Abondances brutes","Occurrences","Proportion","Nombre de sites")
           ,
           tendanceSurFigure=TRUE
           ,
           tendanceGroupSpe = FALSE
           ,
           seuilSignif=0.05
           ,
           seuilAbond=NA
           ,
           ecritureStepByStep=TRUE
           ,
           doBeep=F
           ,
           VarEffect=VarEffectI
           ,
           RandomEffect=RandomEffectI
          ) 

}
beep()
