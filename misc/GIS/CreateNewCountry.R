library(raster)
library(rgdal)

World=shapefile("C:/Users/yvesb/Documents/SIG/Countries/World_L93.shp")
Country="Liechtenstein"


head(World)

SubW=subset(World,World$NAME_ENGL==Country)
dim(SubW)
plot(SubW)

writeOGR(SubW,dsn="C:/Users/yvesb/Documents/VigieChiro/GIS"
                   ,layer=Country
                  ,driver="ESRI Shapefile",overwrite=T)
