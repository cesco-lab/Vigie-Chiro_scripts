library(data.table)

tabase=fread("RSDB_HF_tabase3HF_sansfiltre.csv")

Buzzes=subset(tabase,tabase$Type=="buzz")

Buzzes$Id[1]=1

test=match("Car170517-2014-Pass1-F1-OB-1_0_20140708_232729_426.wav"
           ,Buzzes$Filename)

Buzzes=Buzzes[order(Buzzes$StTime),]
Buzzes=Buzzes[order(Buzzes$Filename),]
Buzzes=Buzzes[order(Buzzes$Espece),]

bi=1
for (i in 2:nrow(Buzzes))
{
  if(Buzzes$Filename[i]==Buzzes$Filename[i-1])
  {
    if(Buzzes$StTime[i]>(Buzzes$StTime[i-1]+500))
    {
      bi=bi+1
    }
  }else{
    bi=bi+1
  }
  Buzzes$Id[i]=bi
  if(i%%1000==2){print(paste(i,bi,Sys.time()))}
}

EcholoB=subset(tabase,tabase$Type %in% c("echolo","buzz"))


Species=unique(EcholoB$Espece)
NbBuz=vector()
RateBuz=vector()
DurBuz_mean=vector()
DurBuz_sd=vector()
FreqBuz_mean=vector()
FreqBuz_sd=vector()
for (j in 1:length(Species))
{
print(Species[j])
    Datai=subset(EcholoB,EcholoB$Espece==Species[j])
  Buzi=subset(Buzzes,Buzzes$Espece==Species[j])
  NbBuzi=length(unique(Buzi$Id))
  NbBuz=c(NbBuz,NbBuzi)
  StSeq=aggregate(Datai$StTime,by=list(Datai$Filename),min)
  EndSeq=aggregate(Datai$StTime+Datai$Dur,by=list(Datai$Filename),max)
  TimeActiv=(EndSeq$x-StSeq$x)/1000
  sum(TimeActiv)
  RateBuzi=NbBuzi/sum(TimeActiv)*3600
  RateBuz=c(RateBuz,RateBuzi)
  if(nrow(Buzi)>0)
  {
  StBuz=aggregate(Buzi$StTime,by=list(Buzi$Id),min)
  EndBuz=aggregate(Buzi$StTime+Buzi$Dur,by=list(Buzi$Id),max)
DurBuzi=EndBuz$x-StBuz$x  
  
DurBuz_mean=c(DurBuz_mean,mean(DurBuzi))
  DurBuz_sd=c(DurBuz_sd,sd(DurBuzi))
FreqBuzi=aggregate(Buzi$FreqMP,by=list(Buzi$Id)
                   ,FUN=function(x) quantile(x,0.5))
  hist(FreqBuzi$x)
MedFB=median(FreqBuzi$x)    
HarmInf=(FreqBuzi$x<(MedFB*0.75))
HarmSup=(FreqBuzi$x>(MedFB*1.5))
FreqBuzi=subset(FreqBuzi,!HarmInf&!HarmSup)
FreqBuz_mean=c(FreqBuz_mean,mean(FreqBuzi$x))
FreqBuz_sd=c(FreqBuz_sd,sd(FreqBuzi$x))
  }else{
    DurBuz_mean=c(DurBuz_mean,NA)
    DurBuz_sd=c(DurBuz_sd,NA)
    FreqBuz_mean=c(FreqBuz_mean,NA)
    FreqBuz_sd=c(FreqBuz_sd,NA)
    
  }
}
TraitsBuzz=data.frame(Species,NbBuz,
                      RateBuz,
                      DurBuz_mean,
                      DurBuz_sd,
                      FreqBuz_mean,
                      FreqBuz_sd)
fwrite(TraitsBuzz,"TraitsBuzz.csv",sep=";")

