library(data.table)
library(Hmisc)

CoordO=c(43.826332, 3.769719) #ld
CoordD=c(44.172846, 5.928672) #sisteron
#CoordD=c(47.462607, -0.556099) #angers
CoordD=c(49.888588, 2.306952) #amiens
CoordD=c(47.877807, -3.915657) #concarneau
CoordD=c(44.567553, 2.004573) #Figeac
CoordD=c(42.618882, 3.004957) #saint-cyprien
CoordD=c(42.948265, 2.551973) #laroque-de-fa
CoordD=c(46.270407, 6.510391) #piero approx
CoordD=c(46.497706, 3.949077) #parents blandine
CoordD=c(47.264734, 5.284675) #estelle & loic
CoordD=c(47.608577, -1.340488) #milou& billy approx
CoordD=c(43.750814, 5.317180) #manu&cha approx
CoordD=c(45.381385, 2.076635) #etienne approx

DirPT="./VigieChiro/gbifData/ST"
PTf=list.files(DirPT,pattern=paste0("ScoreTab"),full.names=T)

ScoreRel=vector()
ScoreAbs=vector()
ScoreRelMax=vector()
ScoreRelDMax=vector()
Day=vector()
for (i in 1:length(PTf))
{
  PTdata=fread(PTf[i]) 
  SMax=max(PTdata$ScoreRatio)
  DFcoordO=cbind(longitude=CoordO[2],latitude=CoordO[1])
  MatchCoord=find.matches(DFcoordO,subset(PTdata,select=c("longitude","latitude")),tol=c(0.1,0.1)
                          ,maxmatch=1)
  SO=PTdata[MatchCoord[[1]][1],]$ScoreRatio
  ScoreRelMax=c(ScoreRelMax,SO/SMax)
  
  Dayi=gsub("ScoreTab","",basename(PTf[i]))
  Dayi=gsub(".csv","",Dayi)
  Day=c(Day,Dayi)
  DFcoordD=cbind(longitude=CoordD[2],latitude=CoordD[1])
  MatchCoord=find.matches(DFcoordD,subset(PTdata,select=c("longitude","latitude")),tol=c(0.1,0.1)
                          ,maxmatch=1)
  SD=PTdata[MatchCoord[[1]][1],]$ScoreRatio
  ScoreRelDMax=c(ScoreRelDMax,SD/SMax)
  
  ScoreRel=c(ScoreRel,SD/SO)
  ScoreAbs=c(ScoreAbs,SD)
if(i%%10==1){print(paste(Sys.time(),i))}
  }

plot(as.numeric(Day),ScoreRel)
plot(as.numeric(Day),ScoreRelMax)
plot(as.numeric(Day),ScoreRelDMax)

#plot(as.numeric(Day),ScoreRel,xlim=c(240,250))
#plot(as.numeric(Day),ScoreRel>median(ScoreRel))
DayD=subset(as.numeric(Day),ScoreRel>median(ScoreRel))
#print(DayD[order(DayD)])
DayM=subset(as.numeric(Day),ScoreRelMax>median(ScoreRelMax))
#print(DayM[order(DayM)])

head(Day[order(ScoreRelDMax,decreasing=T)],20)

         