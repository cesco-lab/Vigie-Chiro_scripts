library(data.table)
FRP="C:/wamp64/www/DataRP_SpTron_50.csv"
FPF="C:/wamp64/www/SpNuit2_50_DataLP_PF_exportTot.csv"
#FTempMean="C:/Users/Yves Bas/Downloads/tg_0.25deg_reg_v17.0.nc"
#AnneeDerniere=2018
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Var="RR"
DayWindow=c(99,0)
LogT=T # to log-transform (e.g. for rainfall) = log(x+1,10)
Raw=T #to compute raw values instead of anomalies (e.g. for rainfall)


FLLU=list.files("./VigieChiro/Weather",pattern=paste0(Var,"_LLU"),full.names=T)
if(length(FLLU)==1)
{
  LLU=fread(FLLU)
}else{
  stop(paste("problem in LLU file number:",length(FLLU)))
 }

WeatherFiles=list.files("./VigieChiro/Weather",pattern=paste0("point_",Var)
                        ,full.names=T)
WeatherFiles=subset(WeatherFiles,nchar(basename(WeatherFiles))==16)

dataWeather=list()
for (h in 1:length(WeatherFiles))
{
  dataWeather[[h]]=fread(WeatherFiles[h],h=T)
  #print(h)
}
Weather=rbindlist(dataWeather)



if(LogT)
{
  Weather[,2:ncol(Weather)]=log(Weather[,2:ncol(Weather)]+1,10)
}
Weather$yday=substr(Weather$date_extract,6,10)

NormWeather=aggregate(Weather[,2:(ncol(Weather)-1)],by=list(Weather$yday)
                      ,FUN=function(x)
                        mean(x,na.rm=T))
plot(NormWeather[,(sample.int((ncol(NormWeather)-1),1)+1)])
CompWeather=merge(Weather,NormWeather,by.x="yday",by.y="Group.1")
RawWeather=CompWeather[,2:(ncol(Weather)-1)]
NormWeatherAll=CompWeather[,(ncol(Weather)+1):ncol(CompWeather)]

AnomWeather=data.table(date_extract=RawWeather$date_extract)
if(!Raw)
{
  for (g in 2:ncol(RawWeather))
  {
    AnomSite=as.data.frame(RawWeather)[,g]-as.data.frame(NormWeatherAll)[,g]
    #boxplot(AnomSite~substr(AnomWeather$date_extract,1,4))
    AnomWeather=cbind(AnomWeather,AnomSite)
    colnames(AnomWeather)[ncol(AnomWeather)]=as.character(g-1)
    if(g%%100==2){print(paste(g,Sys.time()))}
  }
}else{
  AnomWeather=cbind(AnomWeather,RawWeather[,2:ncol(RawWeather)])
  S1=colnames(AnomWeather)[1]
  colnames(AnomWeather)=gsub(".x","",colnames(AnomWeather))
  colnames(AnomWeather)[1]=S1
  
}
AnomWeather=AnomWeather[order(AnomWeather$date_extract),]

#m1=lm(AnomSite~as.numeric(substr(AnomWeather$date_extract,1,4)))
#summary(m1)

#DManquantes=apply(Weather,MARGIN=1,FUN=function(x) sum(is.na(x)))
#boxplot(DManquantes~Weather$month)

SpNuit=fread(FPF)
SpTron=fread(FRP)
ListPart=levels(as.factor(SpNuit$participation))

PartPF=subset(Particip,Particip$participation %in% ListPart)
SLP=merge(PartPF,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"),all=FALSE)
SpNuit_SLP=merge(SpNuit,SLP,by="participation")

ListPartRP=levels(as.factor(SpTron$participation))
PartRP=subset(Particip,Particip$participation %in% ListPartRP)
SiteRP=unique(SiteLoc,by="site")
SLRP=merge(PartRP,SiteRP,by="site",all=FALSE)
SpTron_SLP=merge(SpTron,SLRP,by="participation")

SpSLP=rbindlist(list(SpNuit_SLP,SpTron_SLP),use.names=T,fill=T)


NuitRP=paste(substr(SpSLP$date_debut,7,10)
             ,substr(SpSLP$date_debut,4,5)
             ,substr(SpSLP$date_debut,1,2)
             ,sep="-"
             
)

SpSLP$Nuit=ifelse(is.na(SpSLP$Nuit),NuitRP,SpSLP$Nuit)

table(substr(SpSLP$site,1,17))

Long25=(floor(SpSLP$longitude*4)/4)+0.125
Lat25=(floor(SpSLP$latitude*4)/4)+0.125
Long25NCEP=(round(SpSLP$longitude*0.4)/0.4)
Lat25NCEP=(round(SpSLP$latitude*0.4)/0.4)


test=paste(Long25,Lat25)
nlevels(as.factor(test))
if(grepl("NCEP",Var))
{
LLDU=unique(cbind(Long25NCEP,Lat25NCEP,SpSLP$Nuit))
}else{
  LLDU=unique(cbind(Long25,Lat25,SpSLP$Nuit))
  
}
#LLDU=subset(LLDU,substr(LLDU[,3],1,4)!=AnneeDerniere)
#LLDU=subset(LLDU,substr(LLDU[,3],1,4)!=substr(Sys.time(),1,4))


Anom=vector()
for (i in 1:nrow(LLDU))
{
  if (i%%100==1){print(paste(i,Sys.time()))}
  MatchLongLat=match(paste(LLDU[i,1],LLDU[i,2])
                     ,paste(LLU$longitude,LLU$latitude))
  MatchDate=match(LLDU[i,3],as.character(AnomWeather$date_extract))
  if(is.na(MatchDate))
  {
    Anomi=NA
  }else{
    Anomi=mean(as.data.frame(AnomWeather)
               [(MatchDate+DayWindow[1]):(MatchDate+DayWindow[2]),(MatchLongLat+1)]
               ,na.rm=T)
    
  }
  Anom=c(Anom,Anomi)
}
boxplot(Anom~substr(LLDU[,3],1,4))
table(substr(LLDU[,3],1,4))
AnomalieDF=cbind(data.frame(LLDU),Anom)
colnames(AnomalieDF)[ncol(AnomalieDF)]=paste0(Var,"_",DayWindow[1],"_"
                                              ,DayWindow[2])
if(LogT)
{
  colnames(AnomalieDF)[ncol(AnomalieDF)]=paste0("Log"
                                                ,colnames(AnomalieDF)
                                                [ncol(AnomalieDF)])
}
if(Raw)
{
  colnames(AnomalieDF)[ncol(AnomalieDF)]=paste0("Raw_"
                                                ,colnames(AnomalieDF)
                                                [ncol(AnomalieDF)])
  
}
fwrite(AnomalieDF,paste0("./VigieChiro/Weather/AnomDF_",colnames(AnomalieDF)[ncol(AnomalieDF)]
                         ,".csv"))




