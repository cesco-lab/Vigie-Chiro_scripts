library(data.table)
Captures=fread("./VigieChiro/GIS/Capture/data_2019-06-06_clean_loc_sunset.csv")

Captures$TAXON[Captures$TAXON=="Myotis latipennis"]="Myotis crypticus"
Captures$TAXON[Captures$TAXON=="Myotis nattereri_Spa"]="Myotis crypticus"

table(Captures$MOIS,Captures$EPID)



ListSp=unique(Captures$TAXON)
MeanGestantes=vector()
MiseBas=vector()
for (i in 1:length(ListSp))
{
  print(ListSp[i])
  Datai=subset(Captures,Captures$TAXON==ListSp[i])
  FG=subset(Datai,(Datai$GESTATION %in% c("G","PG"))&!is.na(Datai$DATE_NIGHT_POSIX))
  FG$gestante=T
  FNG=subset(Datai,!Datai$GESTATION %in% c("G","PG")&!is.na(Datai$DATE_NIGHT_POSIX))
  
  dayFG=round(mean(yday(FG$DATE_NIGHT_POSIX),na.rm=T))
  MeanGestantes=c(MeanGestantes,dayFG)
  FLact=subset(FNG,FNG$MAMELLES %in% c("M2","M3"))
  FLact$gestante=F
  FGL=rbind(FG,FLact)
  if(nrow(FG)>0)
  {
  mod=glm(FGL$gestante~yday(FGL$DATE_NIGHT_POSIX),family="binomial")
Inflexion=-mod$coefficients[1]/mod$coefficients[2]
MiseBas=c(MiseBas,round(Inflexion))    
  }else{
    MiseBas=c(MiseBas,min(yday(FLact$DATE_NIGHT_POSIX)))
  }
}
DataMiseBas=data.frame(ListSp,MiseBas,MeanGestantes)
fwrite(DataMiseBas,"DataMiseBas.csv",sep=";")
