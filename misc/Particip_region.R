library(data.table)
library(raster)

Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv",encoding="UTF-8")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.txt")
FranceDep=shapefile("C:/Users/yvesb/Documents/SIG/Limite_administrative/France_dep_L93.shp")
AncienneRegion=c("LANGUEDOC-ROUSSILLON",       "RHONE-ALPES"     ,           "ILE-DE-FRANCE"             
                 , "CENTRE"  ,                   "PROVENCE-ALPES-COTE-D'AZUR", "PICARDIE"                  
                 , "BRETAGNE"        ,           "AUVERGNE"        ,           "CHAMPAGNE-ARDENNE"         
                 , "NORD-PAS-DE-CALAIS"      ,   "MIDI-PYRENEES"      ,        "BOURGOGNE"                 
                 , "PAYS-DE-LA-LOIRE"         ,  "ALSACE"            ,         "CORSE"                     
                 ,         "BASSE-NORMANDIE"    ,        "AQUITAINE"                 
                 , "LIMOUSIN"         ,          "FRANCHE-COMTE"       ,       "POITOU-CHARENTE"           
                 , "LORRAINE"        ,           "HAUTE-NORMANDIE"           )
NouvelleRegion=c("OCCITANIE","ARA","ILE-DE-FRANCE","CENTRE","PACA","HDF","BRETAGNE","ARA","GRAND-EST"
                 ,"HDF","OCCITANIE","BFC","PDL","GRAND-EST","CORSE","NORMANDIE","NOUVELLE-AQUITAINE"
                 ,"NOUVELLE-AQUITAINE","BFC","NOUVELLE-AQUITAINE","GRAND-EST","NORMANDIE")

cbind(AncienneRegion,NouvelleRegion)

FranceW84=spTransform(FranceDep,CRS("+init=epsg:4326"))

SiteLocS=unique(SiteLoc,by=c("site"))
coordinates(SiteLocS)=c("longitude","latitude")

DataSL=intersect(SiteLocS,FranceW84)
table(DataSL$NOM_RégION)

matchSL=match(Particip$site,DataSL$site)
summary(is.na(matchSL))
head(subset(Particip,is.na(matchSL))$site)
     table(substr(subset(Particip,is.na(matchSL))$site,1,17))
Particip$region=DataSL$NOM_RégION[matchSL] 
unique(Particip$region)
table(Particip$region)
testRnew=match(Particip$region,AncienneRegion)
Particip$region2=NouvelleRegion[testRnew]
table(Particip$region2)
fwrite(Particip,"Particip_region.csv",sep=";")
