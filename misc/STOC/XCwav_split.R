library(tuneR)
#library(seewave)

DirWav="D:/test"
OnlyRecent=F

SplitDir=paste0(DirWav,"/split",substr(Sys.time(),1,10),"/")
dir.create(SplitDir)

ListW=list.files(DirWav,pattern=".wav$",full.names=T)


InfoW=file.info(ListW)
TimeW=Sys.time()-InfoW$ctime
if(OnlyRecent)
{
ListWrecent=subset(ListW,TimeW<15000)
}else{
  ListWrecent=ListW
}

#j=1
for (j in 1:length(ListWrecent))
{
  Dur=0
  if((file.size(ListWrecent[j]))>50000)
  {
    tempW=readWave(ListWrecent[j])
    Dur=length(tempW@left)/tempW@samp.rate
    if(Dur>0)
    {
      for(k in 1:ceiling(Dur/5))
      {
        tempCL=extractWave(channel(tempW,which="left")
                           ,from=(k-1)*5,to=min(Dur,k*5)
                           ,xunit="time")
        #tempCL=cutw(channel(tempW,which="left"),from=(k-1)*5,to=min(Dur,k*5),output="Wave")
        tempCL=normalize(tempCL,level=0.3,unit="16")
        writeWave(tempCL,filename=paste0(SplitDir
                         ,substr(basename(ListWrecent[j]),1
                                 ,nchar(basename(ListWrecent[j]))-4)
                                         ,"-L",k,".wav")
                  ,extensible=T)
        #savewav(tempCL,filename=paste0(SplitDir,substr(basename(ListWrecent[j]),1,nchar(basename(ListWrecent[j]))-4),"-L",k,".wav"))
        if(length(tempW@right)>0){
          tempCR=extractWave(channel(tempW,which="right")
                             ,from=(k-1)*5,to=min(Dur,k*5)
                             ,xunit="time")
          tempCR=normalize(tempCR,level=0.3,unit="16")
          writeWave(tempCR
                    ,filename=paste0(SplitDir
                                  ,substr(basename(ListWrecent[j]),1
                                       ,nchar(basename(ListWrecent[j]))-4)
                                           ,"-R",k,".wav")
                    ,extensible=F)
        }
      }
    }}
  print(paste(j,ListWrecent[j],Dur))
  
}
ListS=list.files(SplitDir,pattern=".wav$",full.names=T)
SelDir=paste0(DirWav,"/sel",substr(Sys.time(),1,10),"/")
dir.create(SelDir)
SelS=sample(ListS,10)
file.copy(from=SelS,to=SelDir)
