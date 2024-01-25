library(raster)
library(data.table)
library(dismo)
library(rgdal)
library(maptools)

ListRef=shapefile("C:/Users/yvesb/Documents/SIG/ReferGCLR.shp")
FranceComC=shapefile("C:/Users/yvesb/Documents/SIG/Limite_administrative/Communes_L93_centroides.shp")
FranceCom=shapefile("C:/Users/yvesb/Documents/SIG/Communes_L93.shp")
ReferentSpec=fread("Référents prospections - secteurs_spec.csv",encoding="UTF-8")
BBox=as.matrix(data.table(LongBox=c(496347.6, 945012.4)
                          ,LatBox=c(6039647.8, 6527231.5)))



ReferentSpec$COMM=toupper(ReferentSpec$commune)

LRcom=subset(FranceCom,FranceCom$'NOM_R\xe9gION'=="LANGUEDOC-ROUSSILLON")
LRcomC=subset(FranceComC,FranceComC$NOM_RégIO=="LANGUEDOC-ROUSSILLON")
Ref_L93=spTransform(ListRef,CRS=proj4string(LRcom))
LRcomC=spTransform(LRcomC,CRS=proj4string(LRcom))
RefV=voronoi(Ref_L93,ext=BBox)
plot(RefV)

ComOver=over(LRcomC,RefV)
LRcomC$Referent=ComOver$Nom
subset(LRcomC$COMMUNE0,is.na(ComOver$Nom))

test0=match(ReferentSpec$`code insee`,LRcomC$COMMUNE)
subset(ReferentSpec$commune,is.na(test0))

test=match(LRcomC$COMMUNE,ReferentSpec$`code insee`)
table(subset(LRcomC$DÃ.pARTEM0,!is.na(test)))
LRcomC$Referent=ifelse(is.na(test),LRcomC$Referent,ReferentSpec$Nom[test])

table(LRcomC$Referent)
subset(LRcomC$COMMUNE0,LRcomC$Referent=="Blandine Carré & Yves Bas")
subset(LRcomC$COMMUNE0,LRcomC$Referent=="Maria Lanzellotti")

fwrite(as.data.frame(LRcomC),"LRcomC.csv",sep=";")

testCom=match(LRcom$COMMUNE,LRcomC$COMMUNE)
#plot(testCom)
LRcom$Referent=LRcomC$Referent[testCom]

LRsimple=as.data.table(LRcom)
LRsimple=subset(LRsimple,select=c("Referent","DépARTEM0","COMMUNE0","COMMUNE"))

fwrite(LRsimple,"./chiros/LRcom_referents.csv")

writeOGR(LRcom,dsn="C:/Users/Yves Bas/Documents/VigieChiro/GIS"
         ,layer="ComRefs.shp"
         ,driver="ESRI Shapefile",overwrite=T)

LRrefs=unionSpatialPolygons(LRcom,LRcom$Referent)
plot(LRrefs)
LRrefs$id=c(1:length(LRrefs))

writeOGR(LRrefs,dsn="C:/Users/Yves Bas/Documents/VigieChiro/GIS"
         ,layer="LR_Refs.shp"
         ,driver="ESRI Shapefile",overwrite=T)

