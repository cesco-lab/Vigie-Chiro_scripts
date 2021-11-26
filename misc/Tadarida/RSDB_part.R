library(data.table)
library(lubridate)

f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 22, nchar(x)-8), ".", substr(x, nchar(x) - 6, nchar(x)-4), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

RSDB=fread("RSDB_HF_tabase3HF_sansfiltre.csv")
Particip=fread("C:/wamp64/www/p_export_forLinux.csv")

RSDB_PF=subset(RSDB,substr(RSDB$Filename,1,3)=="Car")

Car=substr(RSDB_PF$Filename,4,9)
Point=tstrsplit(RSDB_PF$Filename,split="-")[[4]]
ListCar=unique(Car)
ListCar=ListCar[order(ListCar)]

Part_RSDB=vector()
for (i in 1:length(ListCar))
{
  print(paste(i,Sys.time()))
  Pointi=subset(Point,Car==ListCar[i])
  ListPoint=unique(Pointi)
  for (j in 1:length(ListPoint))
  {
    RSDBij=subset(RSDB_PF,(Car==ListCar[i])&(Point==ListPoint[j]))
    Partij=subset(Particip,(Particip$site==paste0("Vigiechiro - Point Fixe-"
                                                  ,ListCar[i]))&
                    (Particip$point==ListPoint[j]))
    Datesij=f2pPF(RSDBij$Filename)
    Dayij=unique(as.Date(Datesij))
    Partij$DD=as.Date(dmy_hm(Partij$date_debut))
    Partij$DF=as.Date(dmy_hm(Partij$date_fin))
    #Filesij=unique(RSDBij$Filename)
    for (k in 1:length(Dayij))
    {
      
      Partijk=subset(Partij,(Partij$DD<=Dayij[k])&(Partij$DF>=Dayij[k]))
      Part_RSDB=c(Part_RSDB,Partijk$participation)
    }
    
  }
  
}
fwrite(data.frame(participation=unique(Part_RSDB)),"Part_RSDB.csv",sep=";")
