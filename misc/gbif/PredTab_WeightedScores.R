library(data.table)
library(readxl)
library(stringr)

GroupeSp=read_xlsx("C:/Users/ybas/Documents/GroupeSp2.xlsx")
#FList=list.files("C:/Users/ybas/Documents/VigieChiro/gbifData/ListSp",full.names=T)
SpeciesAll=fread("SpeciesAll.csv",h=T,sep=";")
DirPT="C:/Users/ybas/Downloads"
DirPTunzip="C:/Users/ybas/Downloads/temp"
FPT=list.files(DirPT,pattern="PredTab_",full.names=T,recursive=T)
ListDay=unique(tstrsplit(basename(FPT),split="_")[[2]])
ListDay="15"
DataListAll=fread("C:/Users/ybas/Downloads/DataListAll.csv")


dir.create(DirPTunzip)

#FPT=FPT[2]
for (h in 1:length(ListDay)){

  #for (h in 4:4){
  Day=ListDay[h]
  #PredTab=fread("C:/Users/Yves Bas/Downloads/PredTab_140_Presence_07_GI_SysGrid__France_Spain_Switzerland_Italy_-2e+06_3e+06_4e+06_8e+06_1e+05.csv")
  FPTD=subset(FPT,grepl(paste0("PredTab_",Day),FPT))
  #FPTD=subset(FPTD,grepl(".csv",FPTD))
  if(sum(grepl(".csv",FPTD))>0){
    FPTD=subset(FPTD,grepl(".csv",FPTD)) 
  }else{
    for (a in 1:length(FPTD)){
      unzip(FPTD[a],exdir=DirPTunzip)

    }
    FPTD=list.files(DirPTunzip,pattern="PredTab_",full.names=T,recursive=T)
    #FPTD=gsub(".zip",".csv",FPTD)  
  }
  #PTlist=list()
  ScoreList=list()
  for (j in 1:length(FPTD))
    #for (j in 1:1)
  {
    #PTlist[[j]]=fread(FPTD[j])
    PredTab=fread(FPTD[j])
    #}
    #PredTab=rbindlist(PTlist)
    
    ScoreTot=rep(0,nrow(PredTab))
    ScoreRatio=rep(0,nrow(PredTab))
    for (i in 1:nrow(GroupeSp))
    {
      #FListGroup=subset(FList,tstrsplit(basename(FList),split="_")[[2]]==GroupeSp$Group[i])
      Groupi=GroupeSp$Group[i]
      Groupi=str_to_title(Groupi)
      Ranki=GroupeSp$Rank[i]
      Ranki=tolower(Ranki)
      match55=match(Ranki,names(DataListAll))
      DataListAll$tomatch=DataListAll[,..match55]
      ListGroup=subset(DataListAll,DataListAll$tomatch==Groupi)
      # if(length(FListGroup)>1)
      # {
      #   stop("problem flist")
      # }else{
      #   if(length(FListGroup)==1)
      #   {
      #     
          #ListGroup=fread(FListGroup)
          ListPurge=subset(ListGroup,ListGroup$species %in% colnames(PredTab))
          ListDV=subset(ListPurge,(ListPurge$species %in% SpeciesAll$Scientific.name))
          ListPurge=subset(ListPurge,!(ListPurge$species %in% SpeciesAll$Scientific.name))
          if(nrow(ListPurge)>0)
          {
            #stop("L71")
            DataGroup=subset(PredTab,select=unique(ListPurge$species))
            ScoreGroup=apply(DataGroup,MARGIN=1,sum)*GroupeSp$Importance[i]
            if(nrow(ListDV)>0)
            {
              DataDV=subset(PredTab,select=unique(ListDV$species))
              
              ScoreDV=apply(DataDV,MARGIN=1,sum)*GroupeSp$Importance[i]
              ScoreRT=ifelse((ScoreGroup+ScoreDV)==0,0,ScoreGroup/(ScoreGroup+ScoreDV))
              #hist(ScoreRT)
              ScoreRT2=ScoreRT*GroupeSp$Importance[i]*nrow(ListPurge)
            }else{
              ScoreRT2=ScoreGroup
            }
            ScoreRatio=ScoreRatio+ScoreRT2
            #print(hist(ScoreGroup,main=GroupeSp$Group[i]))
            ScoreTot=ScoreTot+ScoreGroup
            hist(ScoreRatio)
            print(GroupeSp$Group[i])
            if(sum(is.na(ScoreRatio))>0){stop("NA")}
          }
    #     }  
    #   }
    }
    #hist(ScoreTot)
    ScoreTab=data.frame(longitude=PredTab$Group.1,latitude=PredTab$Group.2,ScoreTot,ScoreRatio)
    ScoreList[[j]]=ScoreTab
  }
  ScoreAll=rbindlist(ScoreList)
  fwrite(ScoreAll,paste0("ScoreTab",Day,"_local.csv"),sep=";")
}

