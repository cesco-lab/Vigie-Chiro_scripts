library(data.table)
library(Hmisc)

DaySave=0
NbPT=1
Coord=c(43.826332, 3.769719) #ld
#Coord=c(43.783011, 3.775645) #lac de la jasse
#Coord=c(43.778200, 3.810646) #A PSL
#Coord=c(43.862178, 3.748876) #aven du bois de haut
#Coord=c(43.856254, 3.716213) #ag valboissiere
#Coord=c(43.881851, 3.730214) #herault sbdp
#Coord=c(43.767558, 3.960661) #sbdm

#Coord=c(43.80767061648823, 3.7538977171842918) #lamalou pres dechett
#Coord=c(44.074945, 3.584194) # plateau aigoual
Coord=c(43.686730, 3.765896) #combaillaux carriere
#Coord=c(43.66169574603482, 3.7898814625184785) #pradas mosson
#Coord=c(43.64846278621399, 3.7606346558990658) #mare bel-air
#Coord=c(43.936456, 3.739733) #laroque
Coord=c(43.742280, 3.763824) #aven viols
#Coord=c(43.910022, 3.839451) #trou fumant
DirPT="."
Day=85
SpeciesAll=fread("SpeciesAll.csv",sep=";")

PTf=list.files(DirPT,pattern=paste0("PredTab_",Day),full.names=T)

if(DaySave!=Day)
{
  my.data=list()
  for (i in 1:min(NbPT,length(PTf)))
  {
    my.data[[i]]=fread(PTf[i])  
  }
  PTdata=rbindlist(my.data)
}
DFcoord=cbind(Group.1=Coord[2],Group.2=Coord[1])
MatchCoord=find.matches(DFcoord,subset(PTdata,select=c("Group.1","Group.2")),tol=c(0.1,0.1)
                        ,maxmatch=1)
Score=PTdata[MatchCoord[[1]][1],]
ScoreT=t(Score)
#ScoreT=ScoreT[order(ScoreT,decreasing=T)]
ScoreR=rank(ScoreT,ties.method = "first")
OrderSp=colnames(Score)[order(ScoreR,decreasing = T)]
OrderSpSel=subset(OrderSp,!(OrderSp %in% SpeciesAll$Scientific.name))
DaySave=Day
head(OrderSpSel,30)

