library(data.table)

Suffix="_Presence_07_GI_SysGrid__France_Spain_Switzerland_Italy_-2e+06_3e+06_4e+06_8e+06_1e+05"
PredFolder="./VigieChiro/ModPred/GBIF/PredPres"


for (h in 16:374)
{
  Date=h
  print(h)
  SelSpDate=fread(paste0("DataSelRare__",Date,".csv"))
  
  #test 129 et 82
  
  PredList=list()
  j=0
  for (i in 1:nrow(SelSpDate))
  {
    FilName=paste0(PredFolder,"/PredPres_",SelSpDate$ListSpValide[i],"_"
                   ,SelSpDate$Group[i],Suffix,".csv")
    
    if(file.exists(FilName))
    {
      Predi=fread(FilName)
      names(Predi)[ncol(Predi)]=SelSpDate$ListSpValide[i]
      j=j+1
      PredList[[j]]=Predi
    }
  }
  
  if(length(PredList)>1)
  {
    PredTab=PredList[[1]]
    for (k in 2:length(PredList))
    {
      PredTab=cbind(PredTab,PredList[[k]][,3])
    }
    PredTab$Score=apply(as.data.frame(PredTab)[,(3:ncol(PredTab))],MARGIN=1,FUN=sum)
  
  fwrite(PredTab,paste0(PredFolder,"/PredTab_",Date,Suffix,".csv"),sep=";")
  PredScore=subset(PredTab,select=c("Group.1","Group.2","Score"))
  fwrite(PredScore,paste0(PredFolder,"/PredScore_",Date,Suffix,".csv"),sep=";")
  }
}