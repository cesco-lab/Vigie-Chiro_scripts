library(data.table)


DirDataGroup="C:/Users/ybas/Documents/VigieChiro/gbifData/RawTest"

ListGroup=list.files(DirDataGroup
                      ,full.names=T,pattern=".csv$")
ListGroup=subset(ListGroup,!grepl("_CoordsU.csv",ListGroup))
ListGroup=subset(ListGroup,!grepl("_simplified.csv",ListGroup))


for (h in 1:length(ListGroup)){
  
  DataGroup2=fread(ListGroup[h])
  DataGroupS=subset(DataGroup2,select=c("phylum","class","order","family"
                                        ,"species","countryCode"
                                        ,"decimalLongitude"
                                        ,"decimalLatitude"
                                        ,"coordinateUncertaintyInMeters"
                                        ,"eventDate"))
  # 
  fwrite(DataGroupS,gsub(".csv","_simplified.csv",ListGroup[h]),sep=";")
}
