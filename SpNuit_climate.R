library(data.table)
library(climateExtract)
library(ncdf4)
FAct="SpNuit2_Seuil50_DataLP_PF_exportTot.csv"  
FTempMean="C:/Users/Yves Bas/Downloads/tg_0.25deg_reg_v17.0.nc"
AnneeDerniere=2018

SpNuit=fread(FAct)
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")

ListPart=levels(as.factor(SpNuit$participation))

PartPF=subset(Particip,Particip$participation %in% ListPart)
SLP=merge(PartPF,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
SpNuit_SLP=merge(SpNuit,SLP,by="participation")

Long25=(floor(SpNuit_SLP$longitude*4)/4)+0.125
Lat25=(floor(SpNuit_SLP$latitude*4)/4)+0.125
test=paste(Long25,Lat25)
nlevels(as.factor(test))

LLU=unique(cbind(Long25,Lat25))
LLDU=unique(cbind(Long25,Lat25,SpNuit_SLP$Nuit))

LLDU=subset(LLDU,substr(LLDU[,3],1,4)!=AnneeDerniere)

LLUdf=data.frame(site_id=c(1:nrow(LLU)),longitude=LLU[,1],latitude=LLU[,2])

Sys.time()
point.TM=point_grid_extract(TempMean,LLUdf)
Sys.time()

Jour=yday(point.TM$date_extract)

AT1=vector()
AT3=vector()
AT9=vector()
AT27=vector()
AT81=vector()
for (i in 1:nrow(LLDU))
{
  if (i%%100==1){print(paste(i,Sys.time()))}
  MatchLongLat=match(paste(LLDU[i,1],LLDU[i,2]),paste(LLU[,1],LLU[,2]))
  MatchDate=match(LLDU[i,3],as.character(point.TM$date_extract))
  T1=point.TM[MatchDate,(MatchLongLat+1)]
  D1=point.TM$date_extract[MatchDate]
  J1=yday(D1)
  J1_30=match(Jour,J1)
  N1=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J1_30)))
  AT1=c(AT1,T1-N1)
  
  T3=mean(point.TM[(MatchDate-2):MatchDate,(MatchLongLat+1)])
  J3=c((J1-2):J1) #last 3 days
  J3=J3-floor((J3-1)/365)*365 #to keep in 1:365 domain
  J3_30=match(Jour,J3)
  N3=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J3_30)))
  AT3=c(AT3,T3-N3)
  
  T9=mean(point.TM[(MatchDate-8):MatchDate,(MatchLongLat+1)])
  J9=c((J1-8):J1) #last 9 days
  J9=J9-floor((J9-1)/365)*365 #to keep in 1:365 domain
  J9_30=match(Jour,J9)
  N9=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J9_30)))
  AT9=c(AT9,T9-N9)
  
  T27=mean(point.TM[(MatchDate-26):MatchDate,(MatchLongLat+1)])
  J27=c((J1-26):J1) #last 9 days
  J27=J27-floor((J27-1)/365)*365 #to keep in 1:365 domain
  J27_30=match(Jour,J27)
  N27=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J27_30)))
  AT27=c(AT27,T27-N27)
  
  T81=mean(point.TM[(MatchDate-80):MatchDate,(MatchLongLat+1)])
  J81=c((J1-80):J1) #last 9 days
  J81=J81-floor((J81-1)/365)*365 #to keep in 1:365 domain
  J81_30=match(Jour,J81)
  N81=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J81_30)))
  AT81=c(AT81,T81-N81)
}

AnomalieTemp=cbind(data.frame(LLDU),AT1,AT3,AT9,AT27,AT81)
fwrite(AnomalieTemp,"AnomalieTemp.csv")


                          

