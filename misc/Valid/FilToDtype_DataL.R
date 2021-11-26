library(data.table)

DataL=fread("C:/wamp64/www/DataL2021-05-31.csv")
FilToD=fread("FiltOD_ParSelShifts2021-05-31.csv")

test=match(DataL$donnee,gsub(".wav","",FilToD$FilToD))
DataL$target=FilToD$Esp[test]
DataL$type=FilToD$Type[test]
fwrite(DataL,paste0("C:/wamp64/www/DataLT",Sys.Date(),".csv"),sep=";")
