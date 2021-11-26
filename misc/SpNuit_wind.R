library(data.table)
#library(climateExtract)
#library(ncdf4)
library(RNCEP)
#FAct="C:/wamp64/www/DataRP_SpTron_90.csv"
FAct="C:/wamp64/www/SpNuit2_50_DataLP_PF_exportTot.csv"
#FTempMean="C:/Users/Yves Bas/Downloads/tg_0.25deg_reg_v17.0.nc"
#AnneeDerniere=2018
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Years=c(1950,2019)
LatWindow=c(41,52)
LongWindow=c(-6,9)
SpNuit=fread(FAct)

ListPart=levels(as.factor(SpNuit$participation))

PartPF=subset(Particip,Particip$participation %in% ListPart)
SLP=merge(PartPF,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"),all=FALSE)
SpNuit_SLP=merge(SpNuit,SLP,by="participation")

Long25=(floor(SpNuit_SLP$longitude*4)/4)+0.125
Lat25=(floor(SpNuit_SLP$latitude*4)/4)+0.125
test=paste(Long25,Lat25)
nlevels(as.factor(test))

LLU=unique(cbind(Long25,Lat25))
LLDU=unique(cbind(Long25,Lat25,SpNuit_SLP$Nuit))

#LLDU=subset(LLDU,substr(LLDU[,3],1,4)!=AnneeDerniere)
LLDU=subset(LLDU,substr(LLDU[,3],1,4)!=substr(Sys.time(),1,4))

LLUdf=data.frame(site_id=c(1:nrow(LLU)),longitude=LLU[,1],latitude=LLU[,2])

#TO DO "rhum.sig995","uwnd.sig995","vwnd.sig995"

point.UW=NCEP.gather("uwnd.sig995","surface"
                     ,months.minmax=c(1,12)
                     ,years.minmax=Years
                     ,lat.southnorth=LatWindow, lon.westeast=LongWindow)

point.UWDT=as.data.table(point.UW)

point.VW=NCEP.gather("vwnd.sig995","surface"
                     ,months.minmax=c(1,12)
                     ,years.minmax=Years
                     ,lat.southnorth=LatWindow, lon.westeast=LongWindow)


point.VWDT=as.data.table(point.VW)

point.TW=(point.UW^2+point.VW^2)^0.5

point.TWT=as.data.table(point.TW)
hist(point.TWT$value)

fwrite(point.TWT,"./VigieChiro/Weather/point_TWT.csv")

point.TM.NCEP=NCEP.gather("air.sig995","surface"
                     ,months.minmax=c(1,12)
                     ,years.minmax=Years
                     ,lat.southnorth=LatWindow, lon.westeast=LongWindow)

point.TMT.NCEP=as.data.table(point.TM.NCEP)
fwrite(point.TMT.NCEP,"./VigieChiro/Weather/point_TMT_NCEP.csv")

