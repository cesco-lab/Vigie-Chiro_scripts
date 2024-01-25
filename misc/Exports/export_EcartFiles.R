library(data.table)

f2pPF <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-0
}


DirData="C:/Users/yvesb/Documents/VigieChiro/Raw"
Out="C:/Users/yvesb/Documents/VigieChiro/Raw/AllEcarts.csv"



Files=list.files(DirData,pattern=".csv$",full.names=T)


FileE=subset(Files,substr(basename(Files),1,7)=="export_")

EcartList=list()
for (i in 1:length(FileE))
{
  DataE=fread(FileE[i])
  if(nrow(DataE)>1){
    print(FileE[i])
    DataE=unique(DataE,by=c("participation","donnee"))  
    
  DataE$time=f2pPF(DataE$donnee)
  DataE$DecTime=c(0,DataE$time[2:nrow(DataE)]-DataE$time[1:(nrow(DataE)-1)])
  DataE$DecTimeF=floor(DataE$DecTime)
  table(DataE$DecTimeF)[order(table(DataE$DecTimeF),decreasing=T)][1:20]
  
  DataE30=subset(DataE,DataE$DecTimeF<=30&DataE$DecTimeF>=0)
  StatEcart=as.data.frame(table(DataE30$participation,DataE30$DecTimeF))
  StatEcart=dcast(StatEcart,"Var1~Var2")
  EcartList[[i]]=StatEcart
  #for (j in 1:15){
    
    
    
  #}
  }
}

AllEcart=rbindlist(EcartList,use.names=T,fill=T)
AllEcart[is.na(AllEcart)]=0

fwrite(AllEcart,Out,sep=";")


