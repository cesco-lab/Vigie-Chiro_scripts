library(data.table)
library(rjson)
library(Hmisc)
library("sp")
library("rgdal")
library(maptools)

takeout="C:/Users/yvesb/Downloads/takeout-20230303T080947Z-001.zip"

unzip(takeout)
StarredPlaces <- fromJSON(file = "./TakeOut/Maps (vos adresses)/Adresses enregistrées.json")
Labels=fromJSON(file = "./TakeOut/Maps/Mes adresses avec libellé/Adresses avec libellé.json")


head(StarredPlaces[[2]][[5]]$`geometry`$`coordinates`)

Long=vector()
Lat=vector()
for (i in 1:length(StarredPlaces$features))
{
  Long=c(Long,StarredPlaces$features[[i]]$properties$Location$`Geo Coordinates`$`Longitude`)
  Lat=c(Lat,StarredPlaces$features[[i]]$properties$Location$`Geo Coordinates`$`Latitude`)
 # if(i%%100==1){print(paste(i,Sys.time()))}
}
Long=as.numeric(Long)
Lat=as.numeric(Lat)
SP=data.frame(Long,Lat)


Long=vector()
Lat=vector()
Label=vector()
for (i in 1:length(Labels$features))
{
  Long=c(Long,Labels$features[[i]]$geometry$coordinates[1])
  Lat=c(Lat,Labels$features[[i]]$geometry$coordinates[2])
  Label=c(Label,Labels$features[[i]]$properties$name)
  # if(i%%100==1){print(paste(i,Sys.time()))}
}
Labels=data.frame(Long,Lat,Label)


MatchGrid=find.matches(SP,cbind(Labels$Long,Labels$Lat),tol=c(0.001,0.001)
                       ,maxmatch = 1)

for (i in 1:nrow(SP)){
  SP$Nom[i]=ifelse(MatchGrid$matches[i]==0,"NA",Labels$Label[MatchGrid$matches[i]])
  
}


Spexport=SP
fwrite(Spexport,"SP.csv",sep=";")

coordinates(Spexport) <- c("Long", "Lat")
proj4string(Spexport) <- CRS("+proj=longlat +datum=WGS84")
writeOGR(Spexport["Nom"], paste0("SP",Sys.Date(),".kml"),layer="Nom", driver="KML") 


kmlPoints(obj=Spexport, kmlfile=paste0("SP",Sys.Date(),".kml"), kmlname="", kmldescription="",
          name=Spexport$Nom, description="",
          icon="http://www.gstatic.com/mapspro/images/stock/962-wht-diamond-blank.png")

#test=st_as_sf(Spexport)


#sf::st_write(obj = test, dsn = paste0("SPf",Sys.Date(),".kml"), driver = "libkml")

