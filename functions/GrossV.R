library(data.table)
library(dismo)
library(sp)
library(deldir)
library(rgdal)

GrossBioc=fread("GrossClim.csv")

GrossA=subset(GrossBioc,!is.na(GrossBioc$bio1))
GrossNA=subset(GrossBioc,is.na(GrossBioc$bio1))


coordinates(GrossA)=c("GrossLong","GrossLat")
proj4string(GrossA) <- CRS("+init=epsg:4326") # WGS 84

coordinates(GrossNA)=c("GrossLong","GrossLat")
proj4string(GrossNA) <- CRS("+init=epsg:4326") # WGS 84

GrossV=voronoi(GrossA,ext=0.1)
#GrossVNA=intersect(GrossNA,GrossV)
#GrossVNAdf=GrossVNA[,21:40]
 #GrossAll=spRbind(GrossA,GrossVNAdf)
 #GrossAll=as.data.frame(GrossAll)

writeOGR(obj=GrossV, dsn=".", layer="GrossV", driver="ESRI Shapefile") # this is in geographical projection
