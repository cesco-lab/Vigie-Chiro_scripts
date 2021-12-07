library(data.table)
library(R.utils)
options(digits.secs=6)

tczDir="C:/Users/yvesb/Documents/www/tczRP"
NperStrata=10
SpeciesList=fread("C:/Users/yvesb/Documents/SpeciesList.csv")
NStrata=10
RSDBout=T
TabRSDB=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/RSDB_HF_tabase3HF_sansfiltre.csv")
#table(DataToSample$protocole)
SpTarget="Nycnoc"
OutF="C:/Users/yvesb/Documents/www/NycnocNegativesRP.csv"


ListStrata=c(0:NStrata)

ListTCZ=list.files(tczDir,pattern=".tar.gz$",full.names=T)

DataSel=data.frame()
for (i in 1:length(ListStrata))
{
  
  NRP=NperStrata
  
  print(ListStrata[i])
  while(NRP>0)
  {
    Parij=sample(ListTCZ,1)
    NamePar=gsub(".tar.gz","",Parij)
    if(!file.exists(NamePar))
    {
      gunzip(Parij,remove=F)
      TarName=gsub(".tar.gz",".tar",Parij)
      untar(TarName,exdir=NamePar)
    }
    ListTCj=list.files(NamePar,full.names=T,pattern=".tc$",recursive=T)
    if(length(ListTCj)>0){
      datatclist=list()
      for (k in 1:length(ListTCj))
      {
        datatclist[[k]]=fread(ListTCj[k])
      }
      DataTC=rbindlist(datatclist)
      DataTC2=subset(DataTC,DataTC$SpMax2!=SpTarget)
      DataSpTarget=subset(DataTC2,select=SpTarget)
      names(DataSpTarget)="SpTarget"
      #test=subset(DataTC2,DataTC2$SuccessProb<DataSpTarget$SpTarget)
      DataSps=subset(DataTC2,select=(subset(names(DataTC2),(names(DataTC2) %in% SpeciesList$Esp))))
      DataTC2$ScoreMax=apply(DataSps,1,max)
      DataTC2$ScoreRel=DataSpTarget$SpTarget/DataTC2$ScoreMax
      plot(DataTC2$ScoreRel,DataSpTarget$SpTarget)
      summary(DataTC2$ScoreRel)
      DataTC2$strata=round(DataTC2$ScoreRel*NStrata)
      Dataij=subset(DataTC2,DataTC2$strata==ListStrata[i])
      if(nrow(Dataij)>0){
        Dataijk=Dataij[sample(nrow(Dataij),1)]
        #print(Dataijk$Group.1)
        #print(Dataijk$participation)
        Dataijk$participation=basename(NamePar)
        #check names and scores
        C1=(substr(Dataijk$Group.1[1],1,3)=="Cir")
        FnameData=tstrsplit(Dataijk$Group.1[1],split="-")
        if(length(FnameData)>4){
          
          
          C2=(substr(FnameData[[3]],1,4)=="Pass")
          C3=(substr(FnameData[[4]],1,4)=="Tron")
          C4=(substr(Dataijk$Group.1[1],nchar(Dataijk$Group.1[1])-7
                     ,nchar(Dataijk$Group.1[1])-7)=="_")
          
          #testproba=Dataijk$SuccessProb[1:min(nrow(Dataijk),10)]
          #testproba2=testproba*100-floor(testproba*100+1e-5)
          #C5=(sum(testproba2)<=0)
          #C6=(substr(Dataijk$donnee[1],1,4)!="Circ")
          
          
          
          if(C1&C2&C3&C4)
          {
            #ToDL=c(ToDL,Dataijk$donnee)
            #PartDL=c(PartDL,Dataijk$participation)
            DataSel=rbindlist(list(DataSel,Dataijk),use.names=T,fill=T)
            NRP=NRP-1
            print(Dataijk$Group.1)
            print(Dataijk$SpMax2)
            print(Dataijk$ScoreMax)
            
            
          }
        }
      }
    }
    
  }
}


if(RSDBout){
  DataSel=subset(DataSel,!(DataSel$Group.1 %in% TabRSDB$Filename))
}

DataSel=DataSel[order(DataSel$participation,DataSel$Group.1),]
DataSel$Req=paste0(DataSel$participation,";",gsub(".wav","",DataSel$Group.1))
fwrite(DataSel,OutF,sep=";")
