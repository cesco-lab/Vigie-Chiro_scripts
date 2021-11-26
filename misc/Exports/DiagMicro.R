library(data.table)


if(!exists("FRaw"))
{
  args=""
  args[4]="./VigieChiro/Raw/DataLP_PF_export_55f.csv"
  args[10]="./VigieChiro/Raw/" #output dir
}


#table "données"
Sys.time()
DataLP=fread(args[4]) # 1e5 lines / sec
Sys.time()


AllCombin=unique(data.frame(Group.1=DataLP$participation
                            ,Group.2=DataLP$DataMicFinal,x=-1))


#ETAPE 0 - tri des participations foireuses (durée séquence Pip)
#A FAIRE : tri sur le sampling rate
Sys.time()
DataPip=subset(DataLP,substr(DataLP$espece,1,3)=="Pip") #3 sec
Sys.time()
if(nrow(DataPip)>0)
{
  DurSeq=DataPip$temps_fin-DataPip$temps_debut
  Q90Pip=aggregate(DurSeq,by=list(DataPip$participation,DataPip$DataMicFinal)
                   ,FUN=function(x) quantile(x,0.9))
  Sys.time()
  Sys.time()
  test=match(paste(AllCombin$Group.1,AllCombin$Group.2)
             ,paste(Q90Pip$Group.1,Q90Pip$Group.2))
  Missing=subset(AllCombin,is.na(test))
  Q90Pip=rbind(Q90Pip,Missing)
  
    
 
}else{

  Q90Pip=AllCombin  
  
}

Q90Pip$probleme_micro=ifelse(Q90Pip$x<=4.3,ifelse(Q90Pip$x<0
                                                  ,"possible","probable")
                             ,"non")


NameFile=paste0(args[10],"/DiagMic_",substr(basename(args[4]),18,20),".csv")

fwrite(Q90Pip,NameFile,sep=";")
