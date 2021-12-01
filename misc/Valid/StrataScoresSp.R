library(data.table)
options(digits.secs=6)

NPF=0
NRPperStrata=10
SpeciesList=fread("C:/Users/yvesb/Documents/SpeciesList.csv")
DataToSample=fread("C:/Users/yvesb/Downloads/NycnocRP.csv")
SelRP=T
StrataWidth=0.1
RSDBout=T
TabRSDB=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/RSDB_HF_tabase3HF_sansfiltre.csv")
#table(DataToSample$protocole)

if(SelRP){
  DataToSample=subset(DataToSample,substr(DataToSample$donnee,1,3)=="Cir")
  #DataToSample=subset(DataToSample,substr(DataToSample$donnee,1,3)=="Cir")
}

if(RSDBout){
  DataToSample=subset(DataToSample,!(DataToSample$donnee %in% gsub(".wav","",TabRSDB$Filename)))
}


DataToSample$strata=round(DataToSample$probabilite/StrataWidth)
table(DataToSample$strata)

ListStrata=unique(DataToSample$strata)
#Routier+Pedestre
ToDL=vector()
PartDL=vector()

for (i in 1:length(ListStrata))
{
  NRP=NRPperStrata
  Datai=subset(DataToSample,DataToSample$strata==ListStrata[i])
  ListPari=unique(Datai$participation)
  print(ListStrata[i])
  while(NRP>0)
  {
    Parij=sample(ListPari,1)
    Dataij=subset(Datai,Datai$participation==Parij)
    Dataijk=Dataij[sample(nrow(Dataij),1)]
    print(Dataijk$donnee)
    #check names and scores
    C1=(substr(Dataijk$donnee[1],1,3)=="Cir")
    FnameData=tstrsplit(Dataijk$donnee[1],split="-")
    if(length(FnameData)>4){
      
      
      C2=(substr(FnameData[[3]],1,4)=="Pass")
      C3=(substr(FnameData[[4]],1,4)=="Tron")
      C4=(substr(Dataijk$donnee[1],nchar(Dataijk$donnee[1])-3
                 ,nchar(Dataijk$donnee[1])-3)=="_")
      
      testproba=Dataijk$probabilite[1:min(nrow(Dataijk),10)]
      testproba2=testproba*100-floor(testproba*100+1e-5)
      C5=(sum(testproba2)<=0)
      C6=(substr(Dataijk$donnee[1],1,4)!="Circ")
      
      
      
      if(C1&C2&C3&C4&C5&C6)
      {
        ToDL=c(ToDL,Dataijk$donnee)
        PartDL=c(PartDL,Dataijk$participation)
        NRP=NRP-1
        
      }
    }
    
  }
  
}

ToImport=paste(PartDL,ToDL,sep=";")
ToImportRP=ToImport[order(ToImport)]
fwrite(data.frame(ToImportRP),"ToImportRPspecies.csv",sep=";")


#Points Fixes
PrioPF=PrioPF[order(PrioPF$Priority,decreasing=T),]
ToDL=vector()
PartDL=vector()
while(NPF>0)
{
  PrefParti=substr(PrioPF$participation[1],1,3)
  eData=fread(paste0(RepDataVC,"/export_",PrefParti,".csv"))
  pData=subset(eData,eData$participation==PrioPF$participation[1])
  
  #check names and scores
  C1=(substr(pData$donnee[1],1,3)=="Car")
  C2=(substr(pData$donnee[1],10,10)=="-")
  C3=(substr(pData$donnee[1],15,15)=="-")
  C4=(substr(pData$donnee[1],16,19)=="Pass")
  C5=(substr(pData$donnee[1],21,21)=="-")
  C6=(substr(pData$donnee[1],24,24)=="-")
  C7=(substr(pData$donnee[1],nchar(pData$donnee[1])-3
             ,nchar(pData$donnee[1])-3)=="_")
  
  testproba=pData$probabilite[1:min(nrow(pData),10)]
  testproba2=testproba*100-floor(testproba*100)
  C8=(sum(testproba2)==0)
  
  if(C1&C2&C3&C4&C5&C6&C7&C8)
  {
    #bat only
    bData=subset(pData,pData$espece %in% BatList$Esp)
    if(nrow(bData)>0){
      for (i in 1:5)
      {
        Time=Sys.time()
        SecTime=substr(Time,18,nchar(as.character(Time)))
        set.seed(as.numeric(SecTime)*1e6)
        SpSampl=sample(unique(bData$espece),1)
        spData=subset(bData,bData$espece==SpSampl)
        Time=Sys.time()
        SecTime=substr(Time,18,nchar(as.character(Time)))
        set.seed(as.numeric(SecTime)*1e6)
        ScSampl=sample(unique(spData$probabilite),1)
        scData=subset(spData,spData$probabilite==ScSampl)
        selData=sample(scData$donnee,1)
        ToDL=c(ToDL,selData)
        PartDL=c(PartDL,PrioPF$participation[1])
      }
    }
    #all species
    for (i in 1:5)
    {
      Time=Sys.time()
      SecTime=substr(Time,18,nchar(as.character(Time)))
      set.seed(as.numeric(SecTime)*1e6)
      SpSampl=sample(unique(pData$espece),1)
      spData=subset(pData,pData$espece==SpSampl)
      Time=Sys.time()
      SecTime=substr(Time,18,nchar(as.character(Time)))
      set.seed(as.numeric(SecTime)*1e6)
      ScSampl=sample(unique(spData$probabilite),1)
      scData=subset(spData,spData$probabilite==ScSampl)
      selData=sample(scData$donnee,1)
      ToDL=c(ToDL,selData)
      PartDL=c(PartDL,PrioPF$participation[1])
    }
    
    NPF=NPF-1
    
  }
  PrioPF=PrioPF[-1,]
  
}

ToImport=paste(PartDL,ToDL,sep=";")
ToImportPF=ToImport[order(ToImport)]
fwrite(data.frame(ToImportPF),"ToImportPFOriginality.csv",sep=";")

