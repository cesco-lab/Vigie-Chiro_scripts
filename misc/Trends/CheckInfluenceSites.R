library(data.table)
library(glmmTMB)
library(car)
library(beepr)
source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))

#DataForTrends=fread("C:/Users/yvesb/Documents/www/sorting_custom.csv")
DataForTrends=fread("C:/Users/yvesb/Downloads/sorting_custom.csv")


head(DataForTrends)
table(DataForTrends$protocole)

DataPF=subset(DataForTrends,DataForTrends$protocole=="POINT_FIXE")
DataPF$Dep=substr(DataPF$site,25,26)
table(DataPF$Dep)


Eptser=subset(DataPF,(DataPF$species=="Eptser")&(DataPF$protocole=="POINT_FIXE"))
#Eptser=Eptser[1:500,]

#Nyclei=subset(DataPF,(DataPF$species=="Nyclei")&(DataPF$protocole=="POINT_FIXE"))



modTrends=glmmTMB(nb_contacts~year+(1|mat)+(1|site/Tron),data=Eptser,family=nbinom2)
summary(modTrends)
beep()


NbCSites=aggregate(Eptser$nb_contacts,by=(c(list(Eptser$site),list(Eptser$point))),sum)
NbCSitesWo0=subset(NbCSites,NbCSites$x>0)
test=match(paste(Eptser$site,Eptser$point),paste(NbCSitesWo0$Group.1,NbCSitesWo0$Group.2))
summary(is.na(test))
EptserWo0=subset(Eptser,!is.na(test))

modTrendsWo0=glmmTMB(nb_contacts~year+(1|mat)+(1|site/Tron),data=EptserWo0,family=nbinom2)
summary(modTrendsWo0)
beep()

Eptser$yscale=scale(Eptser$year)
modTrendsys=glmmTMB(nb_contacts~yscale+(1|mat)+(1|site:Tron),data=Eptser,family=nbinom2)
summary(modTrendsys)
beep()


NbYearSite=aggregate(Eptser$participation,by=c(list(Eptser$site),list(Eptser$Tron),list(Eptser$year)),length)
NbYearSite2=aggregate(NbYearSite$x,by=c(list(NbYearSite$Group.1),list(NbYearSite$Group.2)),length)
Only1=subset(NbYearSite2,NbYearSite2$x==1)

test=match(paste(Eptser$site,Eptser$Tron),paste(Only1$Group.1,Only1$Group.2))
EptserMY=subset(Eptser,is.na(test))

Carre=vector()
Point=vector()
NbN=vector()
Trend=vector()
NbY=vector()
DeltaY=vector()
for (i in 1:length(unique(EptserMY$site)))
{
  print(unique(EptserMY$site)[i])
  Datai=subset(EptserMY,EptserMY$site==unique(EptserMY$site)[i])
  for (j in 1:length(unique(Datai$Tron)))
  {
    Dataij=subset(Datai,Datai$Tron==unique(Datai$Tron)[j])
  Carre=c(Carre,unique(EptserMY$site)[i])
  Point=c(Point,unique(Datai$Tron)[j])
  NbN=c(NbN,nrow(Dataij))
  if(length(unique(Dataij$mat))>1){
  modTrendsij=glmmTMB(nb_contacts~year+(1|mat),data=Dataij,family=nbinom2)
  }else{
    modTrendsij=glmmTMB(nb_contacts~year,data=Dataij,family=nbinom2)
  }
  summary(modTrendsij)
  Trend=c(Trend,summary(modTrendsij)$coefficients$cond[2,1])
  NbY=c(NbY,length(unique(Dataij$year)))
  DeltaY=c(DeltaY,max(Dataij$year)-min(Dataij$year))
  }
}

DataRawCarre=data.frame(Carre,Point,Trend,NbN,NbY,DeltaY)
DataRawCarre$Score=DataRawCarre$Trend*(DataRawCarre$NbN/sd(DataRawCarre$NbN)+
                                               DataRawCarre$NbY/sd(DataRawCarre$NbY)+
  DataRawCarre$DeltaY/sd(DataRawCarre$DeltaY))

fwrite(DataRawCarre,"DataRawCarre.csv",sep=";")
#Sys.time()
#InfluenceTrends <- influence_mixed(modTrends, groups="site") #5 sec / 200 obs ; 401 sec / 2000 obs ; 32 sec / 500 ob
#Sys.time()
#beep()


ListSite=unique(Eptser$site)
ListDep=unique(Eptser$Dep)

#Nsite=30
Trends=vector()
InfluenceTrendsSite=list()
for (i in 1:length(ListDep)){
  Sys.sleep(1)
  print(ListDep[i])
  EptserDep=subset(Eptser,Eptser$Dep==ListDep[i])
  if(length(unique(EptserDep$mat))==1){
    modTrends=glmmTMB(nb_contacts~year+(1|site:Tron),data=EptserDep,family=nbinom2)
    
  }else{
    
    modTrends=glmmTMB(nb_contacts~year+(1|mat)+(1|site:Tron),data=EptserDep,family=nbinom2)
  }
  summary(modTrends)
  print(length(unique(EptserDep$site)))
  if(length(unique(EptserDep$site))>1){
  print(Sys.time())
  InfluenceTrendsDep <- try(influence_mixed(modTrends, groups="site")) #5 sec / 200 obs ; 401 sec / 2000 obs ; 32 sec / 500 ob
  print(Sys.time())
  if(class(InfluenceTrendsDep)=="try-error")
  {
    modTrends=glmmTMB(nb_contacts~year+(1|site:Tron),data=EptserDep,family=nbinom2)
    print(Sys.time())
    InfluenceTrendsDep <- (influence_mixed(modTrends, groups="site")) #5 sec / 200 obs ; 401 sec / 2000 obs ; 32 sec / 500 ob
    print(Sys.time())
    }
  
  beep()
  InfluenceTrendsSite[[i]]=InfluenceTrendsDep
  }
  Trends=c(Trends,summary(modTrends)$coefficients$cond[2,1])
  
}

#InfluenceAll=rbindlist(InfluenceTrendsSite)
#Slope=vector()
#for (i in 1:length(InfluenceTrendsSite))
#{
 # print(ListDep[i])
  #print(InfluenceTrendsSite[[i]]$fixed.effects[2])
  #Slope=c(Slope,InfluenceTrendsSite[[i]]$fixed.effects[2])  
  
#}
car::infIndexPlot(InfluenceTrendsSite[[1]])

DataCarreList=list()
for (i in 1:length(ListDep)){
TrendMoinsi=InfluenceTrendsSite[[i]]$'fixed.effects[-site]'[,2]
Carre=row.names(InfluenceTrendsSite[[i]]$'fixed.effects[-site]')
Datai=data.frame(Carre,TrendMoinsi,TrendAll=InfluenceTrendsSite[[i]]$fixed.effects[2])
DataCarreList[[i]]=Datai

  
  }
DataAll=rbindlist(DataCarreList)
fwrite(DataAll,"DataAllCarre.csv",sep=";")

DataDep=data.frame(ListDep,Trends)
fwrite(DataDep,"DataDep.csv",sep=";")





Summ1=aggregate(DataForTrends$nb_contacts,by=list(DataForTrends$espece),sum)

data2010=subset(DataForTrends,DataForTrends$year==2011)

data2010site=unique(data2010,by="site")
table(data2010site$mat)


boxplot(DataForTrends$score_max~DataForTrends$espece)

Rhifer=subset(DataForTrends,DataForTrends$species=="Rhifer")

plot(Rhifer$nb_contacts_strict,Rhifer$nb_contacts)
sum(Rhifer$nb_contacts)
RhiferPresence=subset(Rhifer,Rhifer$nb_contacts>0)

test=aggregate(RhiferPresence$nb_contacts_strict,by=list(RhiferPresence$mat),mean)



table(Rhifer$protocole)

summary(DataForTrends$temperature_debut)
