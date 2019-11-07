library(data.table)
#library(archive)
ListTCZ=list.files("C:/wamp64/www/tc_sel2",full.names=T)
#Particip=fread("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/p_export.csv")
SpeciesList=fread("SpeciesList.csv")
DataPb=fread("C:/wamp64/www/DataPb181207.csv")
VSP=fread("ValidSpRares.csv")
ColPrior=c("Group.1","OrderNum","SpMaxF2","SuccessProb","Sel"
           ,"FreqM","FreqC","Tstart","Tend","NbCris","DurMed")

IdPartSelTot=data.frame()
IdSel=data.frame()
for (i in 1:length(ListTCZ))
{
  
  FI=file.info(ListTCZ[i])
  
  if(FI$size>100)
  {
    IdSeltemp=data.frame()
    
    
    data_path <- ListTCZ[i]
    x=untar(data_path,list=T)
    untar(data_path)
        #x <- archive::archive_read(archive = data_path)
    my.data=list()
    for (j in 1:length(x))
    {
      my.data[[j]] <- fread(x[j])
      if(j%%1000==1){print(paste(j,Sys.time()))}
    }
    IdTot=rbindlist(my.data)
    print(IdTot$Group.1[1])
    print(nrow(IdTot))
    IdPartSelTot=rbind(IdPartSelTot,IdTot)
    ListSp=subset(names(IdTot),names(IdTot) %in% SpeciesList$Esp)
    Target=round(log(nrow(IdTot),5))+1
    #look if a file has been tagged within the participation
    IdSel1=subset(IdTot,IdTot$Group.1 %in% paste0(DataPb$donnee,".wav"))    
    IdSel11=IdSel1[sample(nrow(IdSel1),1),]
    if(nrow(IdSel11)>0){print(IdSel11$SpMaxF2[1])}
    IdSeltemp=rbind(IdSeltemp,IdSel11)
    
    #look if the participation has been tagged for potential rare species
    VSPtemp=subset(VSP,VSP$PartSel==substr(basename(ListTCZ[i]),1
                                           ,nchar(basename(ListTCZ[i]))-7))
    if(nrow(VSPtemp)>0){
      for (k in 1:nrow(VSPtemp))
      {
        ColSp=match(VSPtemp$SpTarget[k],names(IdTot))
        IdSel3=subset(IdTot,as.data.frame(IdTot)[,ColSp]==
                        max(as.data.frame(IdTot)[,ColSp]))
        IdSel31=IdSel3[sample(nrow(IdSel3),1),]
        print(IdSel31$SpMaxF2)
        IdSeltemp=rbind(IdSeltemp,IdSel31)
      }
    }
    
    while(nrow(IdSeltemp)<Target)
    {
      
      if(nrow(IdSeltemp)%%2==0)
      {
        Idtemp=IdTot
        while(nrow(Idtemp)>1)
        {
          SelEsp=sample(ListSp,1)
          #print(SelEsp)
          ColEsp=match(SelEsp,names(IdTot))
          LevConf=round(as.data.frame(Idtemp)[,ColEsp]*20)
          #print(table(LevConf))
          SelLC=sample(levels(as.factor(LevConf)),1)
          #print(SelLC)
          # if(as.numeric(as.character(SelLC))>=sample(c(0:20),1))
          #{
          Idtemp=subset(Idtemp,round(as.data.frame(Idtemp)[,ColEsp]*20)==
                          as.numeric(as.character(SelLC)))
          # print(table(Idtemp$SpMaxF2))
          summary(Idtemp$Tend-Idtemp$Tstart)
          #}
        }
        #Idtemp
        print(Idtemp$SpMaxF2)
        IdSeltemp=rbind(IdSeltemp,Idtemp)
      }else{
        
        
        Idtemp=IdTot
        SelEsp0=sample(levels(as.factor(Idtemp$SpMaxF2)),1)
        Idtemp=subset(Idtemp,Idtemp$SpMaxF2==SelEsp0)
        #Listtemp=levels(as.factor(Idtemp$SpMaxF2))
        while(nrow(Idtemp)>1)
        {
          SelEsp=sample(ListSp,1)
          #SelEsp=sample(Listtemp,1)
          
          #print(SelEsp)
          ColEsp=match(SelEsp,names(IdTot))
          LevConf=round(as.data.frame(Idtemp)[,ColEsp]*20)
          #print(table(LevConf))
          SelLC=sample(levels(as.factor(LevConf)),1)
          #print(SelLC)
          # if(as.numeric(as.character(SelLC))>=sample(c(0:20),1))
          #{
          Idtemp=subset(Idtemp,round(as.data.frame(Idtemp)[,ColEsp]*20)==
                          as.numeric(as.character(SelLC)))
          # print(table(Idtemp$SpMaxF2))
          summary(Idtemp$Tend-Idtemp$Tstart)
          #}
        }
        #Idtemp
        print(Idtemp$SpMaxF2)
        IdSeltemp=rbind(IdSeltemp,Idtemp)
      }
      
    }
    IdSel=rbind(IdSel,IdSeltemp)
  }
   
}

IdSelU=unique(IdSel)
IdSelU=IdSelU[order(IdSelU$Group.1),]
IdSelU$donnee=substr(IdSelU$Group.1,1,nchar(IdSelU$Group.1)-4)
fwrite(IdSelU,"IdSel.csv")  
testSelU=(IdPartSelTot$Group.1 %in% IdSelU$Group.1)
IdPartSelTot$Sel=testSelU

IPST1=subset(IdPartSelTot,select=ColPrior)
Col2=subset(names(IdPartSelTot),!names(IdPartSelTot) %in% ColPrior)
IPST2=subset(IdPartSelTot,select=Col2)
IdPartSelTot=cbind(IPST1,IPST2)
IdPartSelTot=IdPartSelTot[order(IdPartSelTot$Group.1,IdPartSelTot$OrderNum),]

fwrite(IdPartSelTot,"IdPartSelTot.csv")
