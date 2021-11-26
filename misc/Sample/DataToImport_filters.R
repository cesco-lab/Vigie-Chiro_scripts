library(data.table)
library(StreamMetabolism)

DataImp=fread("./www/DataToImport5000C.csv")
MonthSel=c("09","10")
MonthSel=NA
HourSel=c("22","23","00","01","02","03","04","05")
HourSel=NA
Output="DataSelCities2.csv"
SiteLoc=fread("./www/sites_localites.txt")
Particip=fread("./www/p_export.csv")
TriDecMin=c(-999,30)
PFonly=T
GIfilter=fread("./www/SitLoc_SpHC1M_12.csv")


f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

if(PFonly){
  DataImp=subset(DataImp,substr(DataImp$Files,1,3)=="Car")
}


DataImp$month=substr(DataImp$Files,nchar(DataImp$Files)-14,nchar(DataImp$Files)-13)
table(DataImp$month)

DataImp$hour=substr(DataImp$Files,nchar(DataImp$Files)-9,nchar(DataImp$Files)-8)
table(DataImp$hour)


if(!is.na(MonthSel))
{
  DataSel=subset(DataImp,DataImp$month %in% MonthSel)
}else{
  DataSel=DataImp
}


if(!is.na(HourSel))
{
  DataSel=subset(DataSel,DataSel$hour %in% HourSel)
}


testP=match(DataSel$Part,Particip$participation)

DataSel$site=Particip$site[testP]
DataSel$point=Particip$point[testP]

if(!is.na(GIfilter))
{
  DataSel=subset(DataSel,paste(DataSel$site,DataSel$point) %in% paste(GIfilter$site.x,GIfilter$nom.x))
}

DateHour=f2pPF(DataSel$Files)
DataSel$Date=as.Date(DateHour)
DataSel$DateHour=as.POSIXct(DateHour)

testS=match(paste(DataSel$site,DataSel$point),paste(SiteLoc$site,SiteLoc$nom))
DataSel$longitude=SiteLoc$longitude[testS]
DataSel$latitude=SiteLoc$latitude[testS]


if(!is.na(TriDecMin[1]))
{
  
  TtSt=vector(length=0)
  TtSr=vector(length=0)
  TimPos=vector(length=0)
  DecMin=vector(length=0)
  for (i in 1:nrow(DataSel))
  {
    Date1=DataSel$DateHour[i]
    if(!is.na(Date1)){
      H1=as.numeric(format(Date1, "%H")) +    as.numeric(format(Date1, "%M"))/60
      Date2=format(as.Date(Date1-43200*(H1<12)),format="%Y/%m/%d")
      Date3=format(as.Date(Date1+43200*(H1>12)),format="%Y/%m/%d")
      St=sunrise.set(DataSel$latitude[i],DataSel$longitude[i],Date2,timezone="CET")[1,2]
      Sr=sunrise.set(DataSel$latitude[i],DataSel$longitude[i],Date3,timezone="CET")[1,1]
      TtSt[i]=difftime(Date1,St,units="mins")
      TtSr[i]=difftime(Sr,Date1,units="mins")
      TimPos[i]=TtSt[i]/as.numeric(difftime(Sr,St,units="mins"))
      DecMin[i]=min(as.numeric(TtSt[i]),as.numeric(TtSr[i]))
      if(i%%10==1){print(paste(i,"/",nrow(DataSel)))}
    }
  }
  DataSel$DecMin=DecMin
  DataSel=subset(DataSel,DataSel$DecMin<TriDecMin[2])
  DataSel=subset(DataSel,DataSel$DecMin>TriDecMin[1])
}

fwrite(DataSel,Output,sep=";")
#DateHour=f2pPF(DataSel$Files)
