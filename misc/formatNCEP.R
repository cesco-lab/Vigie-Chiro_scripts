library(data.table)
NCEPdata=fread("./VigieChiro/Weather/point_TWT.csv")
Tag="W"

WeatherNCEP=dcast(NCEPdata,V3~paste(V1,V2),value.var="value")
WeatherNCEP$date_extract=gsub("_","-",substr(WeatherNCEP$V3,1,10))
WeatherNCEP=as.data.frame(WeatherNCEP)
WeatherNight=WeatherNCEP
WeatherNight[(1:(nrow(WeatherNight)-2)),(1:(ncol(WeatherNight)-1))]=
  WeatherNCEP[(3:(nrow(WeatherNCEP))),(1:(ncol(WeatherNight)-1))]

#make LLU files
site_id=c(1:(ncol(WeatherNCEP)-2))
LongLat=tstrsplit(colnames(WeatherNCEP)[2:(ncol(WeatherNCEP)-1)]
                         ,split=" ")
longitude=LongLat[[2]]
latitude=LongLat[[1]]
LLU=data.frame(site_id,longitude,latitude)
fwrite(LLU,paste0("./VigieChiro/Weather/D",Tag,"M_NCEP_LLUdf.csv"),sep=";")
fwrite(LLU,paste0("./VigieChiro/Weather/D",Tag,"N_NCEP_LLUdf.csv"),sep=";")
fwrite(LLU,paste0("./VigieChiro/Weather/D",Tag,"X_NCEP_LLUdf.csv"),sep=";")
fwrite(LLU,paste0("./VigieChiro/Weather/N",Tag,"M_NCEP_LLUdf.csv"),sep=";")
fwrite(LLU,paste0("./VigieChiro/Weather/N",Tag,"N_NCEP_LLUdf.csv"),sep=";")
fwrite(LLU,paste0("./VigieChiro/Weather/N",Tag,"X_NCEP_LLUdf.csv"),sep=";")


DM=aggregate(WeatherNCEP[,(2:(ncol(WeatherNCEP)-1))]
              ,by=list(WeatherNCEP$date_extract),FUN=mean)
DN=aggregate(WeatherNCEP[,(2:(ncol(WeatherNCEP)-1))]
             ,by=list(WeatherNCEP$date_extract),FUN=min)
DX=aggregate(WeatherNCEP[,(2:(ncol(WeatherNCEP)-1))]
             ,by=list(WeatherNCEP$date_extract),FUN=max)

NM=aggregate(WeatherNight[,(2:(ncol(WeatherNight)-1))]
             ,by=list(WeatherNight$date_extract),FUN=mean)
NN=aggregate(WeatherNight[,(2:(ncol(WeatherNight)-1))]
             ,by=list(WeatherNight$date_extract),FUN=min)
NX=aggregate(WeatherNight[,(2:(ncol(WeatherNight)-1))]
             ,by=list(WeatherNight$date_extract),FUN=max)

colnames(DM)[1]="date_extract"
colnames(DN)[1]="date_extract"
colnames(DX)[1]="date_extract"
colnames(NM)[1]="date_extract"
colnames(NN)[1]="date_extract"
colnames(NX)[1]="date_extract"

fwrite(DM,paste0("./VigieChiro/Weather/point_D",Tag,"M_NCEP.csv"),sep=";")
fwrite(DN,paste0("./VigieChiro/Weather/point_D",Tag,"N_NCEP.csv"),sep=";")
fwrite(DX,paste0("./VigieChiro/Weather/point_D",Tag,"X_NCEP.csv"),sep=";")
fwrite(NM,paste0("./VigieChiro/Weather/point_N",Tag,"M_NCEP.csv"),sep=";")
fwrite(NN,paste0("./VigieChiro/Weather/point_N",Tag,"N_NCEP.csv"),sep=";")
fwrite(NX,paste0("./VigieChiro/Weather/point_N",Tag,"X_NCEP.csv"),sep=";")
