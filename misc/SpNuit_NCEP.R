library(data.table)
#library(climateExtract)
#library(ncdf4)
library(RNCEP)
Years=c(1950,2019)
LatWindow=c(41,52)
LongWindow=c(-6,9)


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
fwrite(point.TMT.NCEP,"./VigieChiro/Weather/point_TNCEP.csv")

