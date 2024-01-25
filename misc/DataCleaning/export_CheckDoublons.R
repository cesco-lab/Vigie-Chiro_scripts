library(data.table)


DirData="C:/Users/yvesb/Documents/VigieChiro/Raw"
Out="C:/Users/yvesb/Documents/VigieChiro/Raw/AllDoublons.csv"



Files=list.files(DirData,pattern=".csv$",full.names=T)


FileE=subset(Files,substr(basename(Files),1,7)=="export_")

PrefList=list()
for (i in 1:length(FileE))
{
  DataE=fread(FileE[i])
  if(nrow(DataE)>1){
    print(FileE[i])
    MetaData=tstrsplit(DataE$donnee,split="-")
    PrefRP=paste(MetaData[[1]],MetaData[[2]],MetaData[[3]])
    PrefPF=paste(MetaData[[1]],MetaData[[2]],MetaData[[3]],MetaData[[4]])
    Pref=ifelse(substr(DataE$donnee,1,3)=="Cir",PrefRP,PrefPF)
    
    PrefA=aggregate(Pref,by=list(DataE$participation),function(x) length(unique(x)))
    print(mean(PrefA$x!=1))
    PrefList[[i]]=PrefA
  }
}
 
AllEcart=rbindlist(PrefList,use.names=T,fill=T)

fwrite(AllEcart,Out,sep=";")


