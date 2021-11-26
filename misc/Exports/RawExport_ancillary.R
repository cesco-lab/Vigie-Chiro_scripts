library(data.table)

FileIn="./Exports/_pourOlivierDuriez_SL.csv"

f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-0
}

Data=fread(FileIn)

Data$carre=tstrsplit(Data$donnee,split="-")[[1]]
Data$point=tstrsplit(Data$donnee,split="-")[[4]]
Data$Time=f2pPF(Data$donnee)
Data$Hour=hour(Data$Time)
Data$Day=mday(Data$Time)
Data$Month=month(Data$Time)

fwrite(Data,gsub(".csv","_ancillary.csv",FileIn),sep=";")

