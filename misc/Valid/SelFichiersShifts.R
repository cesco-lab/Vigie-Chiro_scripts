library(data.table)
library(R.utils)
options(digits.secs=6)

FPart="ParSelShifts2022-04-07.csv"
PrefRequest="pscp -pw PghgEESz1718! ybas@cca.in2p3.fr:/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/tcz/"
BBScorr=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/BBScorr.csv")
NperPart=1 #N
Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv",encoding="UTF-8")
RP=F
#RP=grepl("RP",FPart)
#OutDir="C:/wamp64/www/tcz/"
OutDir="C:/Users/yvesb/Documents/www/tcz/"

PartMissingSp=fread(FPart)


FilToD=vector()
Part=vector()
Esp=vector()
Espinit=""
Type=vector()
for (i in 1:nrow(PartMissingSp))
{
  Parti=PartMissingSp$ParSel[i]
  if(Espinit!=PartMissingSp$ParSel[i])
  {
    np=0
    Espinit=PartMissingSp$ParSel[i]
  }
  Request=paste0(PrefRequest,Parti,".tar.gz ",OutDir,Parti,".tar.gz")  
  if(!file.exists(paste0(OutDir,Parti,".tar.gz")))
  {
    system(Request)
  }
  if(!file.exists(paste0(OutDir,Parti,".tar")))
  {
    gunzip(paste0(OutDir,Parti,".tar.gz"))
  }
  if(!dir.exists(paste0(OutDir,Parti)))
  {
    untar(paste0(OutDir,Parti,".tar")
          ,exdir=paste0(OutDir,Parti))
  }
  ListTC=list.files(paste0(OutDir,Parti),full.names=T,recursive=T)
  if(length(ListTC)>0)
  {
    C1=ifelse(RP,grepl("Tron",basename(ListTC[1]))
              ,substr(basename(ListTC[1]),10,10)=="-")
    #C2=ifelse(RP,substr(basename(ListTC[1]),1,3)=="Cir"
    #         ,substr(basename(ListTC[1]),1,3)=="Car")
    NumSite=ifelse(RP
                   ,gsub("Cir","",tstrsplit(basename(ListTC[1])
                                            ,split="-")[[1]])
                   ,gsub("Car","",tstrsplit(basename(ListTC[1])
                                            ,split="-")[[1]]))
    Partii=subset(Particip,Particip$participation==Parti)
    C2=grepl(NumSite,Partii$site)
    C3=(substr(basename(ListTC[1]),nchar(basename(ListTC[1]))-6
               ,nchar(basename(ListTC[1]))-6)=="_")
    #C4=(np<NperSp)
    InfoTC=tstrsplit(basename(ListTC[1]),split="-")
    C5=(length(InfoTC)>3)
    if(C5)
    {
      Tron=tstrsplit(basename(ListTC[1]),split="-")[[4]]
      C5=ifelse(RP,substr(Tron,5,5)!="0",T)
    }
    if(C1&C2&C3&C5)
    {
      print(PartMissingSp$Esp[i])
      print(PartMissingSp$type[i])
      datalist=list()
      for (j in 1:length(ListTC))
      {
        datalist[[j]]=fread(ListTC[j])
      }
      dataTC=rbindlist(datalist)
      
      dataDispo=dataTC
      ksel=0
      for (k in 1:NperPart)
      {
        if(PartMissingSp$Esp[i] %in% names(dataTC))
        {
          
          Spmax=subset(dataDispo,dataDispo$SpMaxF2==PartMissingSp$Esp[i])
          if(nrow(Spmax)>0)
          {
            ScoreSp=subset(Spmax,select=PartMissingSp$Esp[i])
            names(ScoreSp)="S"
            Best=subset(Spmax,ScoreSp$S==max(ScoreSp$S))
            Good=Spmax
            
          }else{
            ScoreSp=subset(dataDispo,select=PartMissingSp$Esp[i])
            names(ScoreSp)="S"
            Best=subset(dataDispo,ScoreSp$S==max(ScoreSp$S))
            Good=subset(dataDispo,ScoreSp$S>(max(ScoreSp$S)/2))
            
          }
          
          BestFiles=unique(Best$Group.1)
          Time=Sys.time()
          SecTime=substr(Time,18,nchar(as.character(Time)))
          set.seed(as.numeric(SecTime)*1e6)
          if(length(BestFiles)>0)
          {
            FilSampl=sample(BestFiles,1)
          print(FilSampl)
          FilToD=c(FilToD,FilSampl)
          Part=c(Part,Parti)
          Esp=c(Esp,PartMissingSp$Esp[i])
          Type=c(Type,PartMissingSp$type[i])
          np=np+1
          NumFS=match(FilSampl,dataDispo$Group.1)
          NumD=max(round(NumFS-0.02*nrow(dataDispo)),0)
          NumF=min(round(NumFS+0.02*nrow(dataDispo)),nrow(dataDispo))
          if(NumD>0)
          {
            dataAvant=dataDispo[1:NumD,]
          }else{
            dataAvant=dataDispo[0,]
          }
          if(NumF<nrow(dataDispo))
          {
            dataApres=dataDispo[NumF:nrow(dataDispo),]
          }else{
            dataApres=dataDispo[0,]
          }
          dataDispo=rbind(dataAvant,dataApres)
          if(nrow(dataDispo)==0)
          {
            dataDispo=dataTC
          }
          dataDispo=subset(dataDispo,!(dataDispo$Group.1 %in% FilToD))
          Good=subset(Good,Good$Group.1 %in% dataDispo$Group.1)
          if(nrow(Good)>0)
          {
            
            
            
            GoodFiles=unique(Good$Group.1)
            #GoodFiles=subset(GoodFiles,GoodFiles!=FilSampl)
            Time=Sys.time()
            SecTime=substr(Time,18,nchar(as.character(Time)))
            set.seed(as.numeric(SecTime)*1e6)
            FilSampl=sample(GoodFiles,1)
            print(FilSampl)
            FilToD=c(FilToD,FilSampl)
            Part=c(Part,Parti)
            Esp=c(Esp,PartMissingSp$Esp[i])
            Type=c(Type,PartMissingSp$type[i])
            np=np+1
          }
          
        }
        }
      }
    }
  }
}

FiltODmissing=data.frame(cbind(FilToD,Part,Esp,Type))
FiltODmissing=unique(FiltODmissing)

if(RP){
  F1=FiltODmissing
  F0=FiltODmissing
  F0$FilToD=gsub("_1_","_0_",F1$FilToD)
  F1$FilToD=gsub("_0_","_1_",F1$FilToD)
  FiltODmissing=rbind(F0,F1)
  FiltODmissing=FiltODmissing[order(FiltODmissing$FilToD),]
}
FiltODmissing$ToD=paste0(FiltODmissing$Part,";",FiltODmissing$FilToD)
head(FiltODmissing)
fwrite(FiltODmissing,paste0("FiltOD_",FPart),sep=";")

