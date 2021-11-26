library(tuneR)

DirW="D:/test/split2020-08-21"

ListWrecent=list.files(DirW,pattern=".wav$",full.names=T)
WNrates=c(10,24,50,99)

for (i in 1:length(WNrates))
{
  
DestMix=gsub("split202",paste0("wn",WNrates[i],"_202"),DirW)
dir.create(DestMix)

for (j in 1:length(ListWrecent))
{
  Dur=0
  if((file.size(ListWrecent[j]))>50000)
  {
    tempW=readWave(ListWrecent[j])
    Dur=length(tempW@left)/tempW@samp.rate
    if(Dur>0)
    {
      wn=noise(kind="white",duration=length(tempW@left),samp.rate=tempW@samp.rate
               ,bit=tempW@bit,pcm=tempW@pcm)
      Mix=tempW+wn*WNrates[i]/100
      Mix=normalize(Mix,unit="16")
      writeWave(Mix,filename=paste0(DestMix,"/",basename(ListWrecent[j])))
      
        #savewav(Mix,filename=paste0(DestMix,"/",basename(ListWrecent[j])))
        }
  }
  print(paste(j,ListWrecent[j],Dur))
  }
  
  

}