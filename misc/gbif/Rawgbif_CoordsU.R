library(data.table)


DirDataGroup="C:/Users/ybas/Documents/VigieChiro/gbifData/RawTest"

ListGroup=list.files(DirDataGroup
                      ,full.names=T,pattern=".csv$")



for (h in 1:length(ListGroup)){
  
  DataGroup2=fread(ListGroup[h])
  DataGroupC=subset(DataGroup2,(!is.na(DataGroup2$decimalLatitude))
                    &(!is.na(DataGroup2$decimalLongitude)))
  Coords=subset(DataGroupC,select=c("decimalLongitude","decimalLatitude"))
  CoordU1=unique(Coords)
  # Coords=round(Coords,4)
  # CoordU2=unique(Coords)
  # 
  fwrite(Coords,gsub(".csv","_CoordsU.csv",ListGroup[h]),sep=";")
}
