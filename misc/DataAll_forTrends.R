library(data.table)
library(lubridate)

SpNuit=fread("C:/Users/yvesb/Downloads/DataToti.csv")
Out="C:/Users/yvesb/Downloads/indicatorRoutine_noData/data/AllPointsFixes"
SampleSites=30

SpNuit$Mat=paste0(substr(SpNuit$enregistreur,1,3),SpNuit$type_micro)
table(SpNuit$Mat)
length(unique(SpNuit$Mat))
SpNuit$species=SpNuit$espece
SpNuit$site=paste(SpNuit$numero_carre,SpNuit$point)
SpNuit$year=SpNuit$annee
SpNuit$session=paste(SpNuit$numero_carre,SpNuit$point,SpNuit$Nuit)

SpNuit=subset(SpNuit,SpNuit$nuit_complete)
SpNuit=subset(SpNuit,SpNuit$probleme_micro=="non")
SpNuit=subset(SpNuit,SpNuit$site!=" ")
SpNuit$dateNuit=ymd(SpNuit$Nuit)
SpNuit$dateNum=yday(SpNuit$dateNuit)

Samples=unique(SpNuit,by=c("site","year"))
S1=aggregate(Samples$year,by=list(Samples$site),length)

fwrite(SpNuit,paste0(Out,"/forTrends.csv",sep=";"))

S1=S1[order(S1$x,decreasing=T),]
SiteSample=S1[1:SampleSites,]

SpSample=subset(SpNuit,SpNuit$site %in% SiteSample$Group.1)
       
fwrite(SpSample,paste0(Out,"/forTrendsSmallSample.csv",sep=";"))

