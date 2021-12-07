library(data.table)
library(readxl)

GroupeSp=read_xlsx("C:/Users/yvesb/Documents/GroupeSp.xlsx")
FList=list.files("C:/Users/yvesb/Documents/VigieChiro/gbifData/ListSp",full.names=T)
SpeciesAll=fread("C:/Users/yvesb/Documents/SpeciesAll.csv",h=T,sep=";")
DirPT="C:/Users/yvesb/Downloads"
FPT=list.files(DirPT,pattern="PredTab_",full.names=T)
ListDay=unique(tstrsplit(basename(FPT),split="_")[[2]])
ListDay="345"

#FPT=FPT[1]
for (h in 1:length(ListDay)){
#for (h in 4:4){
Day=ListDay[h]
#PredTab=fread("C:/Users/Yves Bas/Downloads/PredTab_140_Presence_07_GI_SysGrid__France_Spain_Switzerland_Italy_-2e+06_3e+06_4e+06_8e+06_1e+05.csv")
FPTD=subset(FPT,grepl(paste0("PredTab_",Day),FPT))

PTlist=list()
#for (j in 1:length(FPTD))
for (j in 1:1)
  {
  PTlist[[j]]=fread(FPTD[j])
}
PredTab=rbindlist(PTlist)

ScoreTot=rep(0,nrow(PredTab))
ScoreRatio=rep(0,nrow(PredTab))
for (i in 1:nrow(GroupeSp))
{
  FListGroup=subset(FList,tstrsplit(basename(FList),split="_")[[2]]==GroupeSp$Group[i])
  if(length(FListGroup)>1)
  {
    stop("problem flist")
  }else{
    if(length(FListGroup)==1)
    {
      
      ListGroup=fread(FListGroup)
      ListPurge=subset(ListGroup,ListGroup$species %in% colnames(PredTab))
      ListDV=subset(ListPurge,(ListPurge$species %in% SpeciesAll$Scientific.name))
      ListPurge=subset(ListPurge,!(ListPurge$species %in% SpeciesAll$Scientific.name))
      if(nrow(ListPurge)>0)
      {
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
    }  
  }
}
#hist(ScoreTot)
ScoreTab=data.frame(longitude=PredTab$Group.1,latitude=PredTab$Group.2,ScoreTot,ScoreRatio)
fwrite(ScoreTab,paste0("ScoreTab",Day,"_local.csv"),sep=";")
}
