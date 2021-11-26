library(R.utils)

DirTA="K:/Yves/Verbatim/1008_MNHN"
NumCar="750023"
NumPoint="Z4"

f2pta <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 21, nchar(x)-7), ".", substr(x, nchar(x) - 5, nchar(x)-3), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

FilTA=list.files(DirTA,full.names=T)
FilTA=subset(FilTA,grepl(".ta.tar",FilTA))

Sys.time()
Datalist=list()
for (i in 1:length(FilTA))
{
  print(paste(Sys.time(),basename(FilTA[i])))
  if(grepl(".bz2",FilTA[i])){
    bunzip2(FilTA[i])
    Sys.time()
  }
  dirTAi=gsub(".bz2","",FilTA[i])
  dirTAi=gsub(".ta.tar","",dirTAi)
  dir.create(dirTAi)
  untar(gsub(".bz2","",FilTA[i]),exdir=dirTAi)
  Sys.time()
  talisti=list.files(dirTAi,pattern=".ta$",full.names=T,recursive=T)
  talistt=f2pta(basename(talisti))
  pourDateNuit=talistt-12*3600 #bricolage-décalage de 12 heures pour ramener à la date du début de nuit
  Sys.time()
  DateNuit=as.Date.POSIXct(pourDateNuit) # date of the beginning of the night
  print(table(DateNuit))
  for (j in 1:length(unique(DateNuit)))
  {
    dir.create(paste0(DirTA,"/",unique(DateNuit)[j]))
  }
  Prefix=paste0("Car",NumCar,"-",substr(DateNuit,1,4),"-Pass1-",NumPoint,"-")
  NewLoc=paste0(DirTA,"/",DateNuit,"/",Prefix,basename(talisti))
  #file.rename(from=talisti,to=NewLoc)
  for (k in 1:length(talisti))
  {
    tak=fread(talisti[k])
    if(nrow(tak)>0){
      tak$Filename=paste0(Prefix[1],tak$Filename)
      fwrite(tak,NewLoc[k],sep="\t")
    }else{
      file.copy(from=talisti[k],to=NewLoc[k])
      #stop("test")
    }
  }
}  
