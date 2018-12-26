library(bioacoustics)
library(tuneR)
args=vector()
args[1]="C:/Users/Yves Bas/Documents/test"
args[2]="C:/Users/Yves Bas/Documents/testww"


  LFwac=list.files(args[1],pattern=".wacs$",full.names=T)

  
  if(length(LFwac)>0)
  {
    dirWS=paste0(args[2])
    dir.create(dirWS)
    
    
Sys.time()

for (i in 1:length(LFwac))
{
    Wtemp=read_wac(LF[i],write_wav = args[2])
}

    for (j in 1:length(Wtemp))
    {
      print(Wtemp[[j]]@stereo)
    }
    
      Dur=round(length(Wtemp[[j]])/Wtemp[[j]]@samp.rate,3)
      if(Dur>0)
      {
        for(k in 1:ceiling(Dur/5))
        {
          tempCL=cutw(channel(tempW,which="left"),from=(k-1)*5,to=min(Dur,k*5),output="Wave")
          tempCL=normalize(tempCL,level=0.3)
          savewav(tempCL,filename=paste0(SplitDir,substr(ListWrecent[j],1,nchar(ListWrecent[j])-4),"-L",k,".wav"))
          if(length(tempW@right)>0){
            tempCR=cutw(channel(tempW,which="right"),from=(k-1)*5,to=min(Dur,k*5),output="Wave",normalize= "16")
            tempCR=normalize(tempCR,level=0.3)
            savewav(tempCR,filename=paste0(SplitDir,substr(ListWrecent[j],1,nchar(ListWrecent[j])-4),"-R",k,".wav"))
          }
        }
      
    }
    
    print(paste(i,Sys.time()))
        
}
Sys.time()
