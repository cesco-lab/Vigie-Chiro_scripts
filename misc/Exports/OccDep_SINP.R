library(data.table)
library(Hmisc)

find_modes<- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] > x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}

SLpred="C:/Users/yvesb/Documents/VigieChiro/gbifData/SL"
OccDir="C:/Users/yvesb/Documents/www/OccDep/220108"
Thres=0.9
NDfilter=T

OccF=list.files(OccDir,pattern="Data",full.names=T)
SLF=list.files(SLpred,full.names=T)

OccF=subset(OccF,!grepl("Sensitive",OccF))
OccF=subset(OccF,!grepl("Confidential",OccF))


OccData=list()
for (i in 1:length(OccF))
{
  OccData[[i]]=fread(OccF[i])
}
OccTot=rbindlist(OccData)

OccTot=subset(OccTot,OccTot$score_max>Thres)
if(NDFilter){
  OccTot=subset(OccTot,OccTot$nb_contacts_nd>0)
}

ListSp=unique(OccTot$espece)

DataClean=data.frame()
MissingSp=vector()
for (j in 1:length(ListSp))
{
  print(j)
  print(ListSp[j])
  OccSp=subset(OccTot,OccTot$espece==ListSp[j])
  FullName=OccSp$NomSci[1]
  print(FullName)
  #FullName=gsub(" ","_",FullName)
  SLsp=subset(SLF,grepl(FullName,SLF))
  if(length(SLsp)==1)
  {
    SLdataj=fread(SLsp)
    OccMatches=find.matches(cbind(OccSp$longitude,OccSp$latitude)
                            ,cbind(SLdataj$Group.1,SLdataj$Group.2)
                            ,tol=c(1,1)
                            ,maxmatch=1)
    OccRange=SLdataj$pred[OccMatches$matches]
    if(length(OccRange)>1){
      RangeDensity=density(OccRange,adjust=2)
      plot(RangeDensity)
      modes=RangeDensity$x[find_modes(RangeDensity$y)]  
      if(length(modes)>1)
      {
        OutRange=subset(OccSp,OccRange<0.2)
        OutRangeV=subset(OutRange,(OutRange$confiance_observateur!="")
                         |(OutRange$confiance_validateur!=""))
        InRange=subset(OccSp,OccRange>=0.2)
        
        #hist(OccRange)
        plot(InRange$longitude,InRange$latitude,xlim=c(-6,10)
             ,ylim=c(41,51),main=ListSp[j])
        points(OutRange$longitude,OutRange$latitude,col=2)
        if(nrow(OutRangeV)>0){
          points(OutRangeV$longitude,OutRangeV$latitude,col=3)
        }
        InRange=rbind(InRange,OutRangeV)
      }else{
        OutRange=subset(OccSp,OccRange<0.1)
        OutRangeV=subset(OutRange,(OutRange$confiance_observateur!="")
                         |(OutRange$confiance_validateur!=""))
        InRange=subset(OccSp,OccRange>=0.1)
        
        #hist(OccRange)
        plot(InRange$longitude,InRange$latitude,xlim=c(-6,10)
             ,ylim=c(41,51),main=ListSp[j])
        points(OutRange$longitude,OutRange$latitude,col=2)
        if(nrow(OutRangeV)>0){
          points(OutRangeV$longitude,OutRangeV$latitude,col=3)
        }
        InRange=rbind(InRange,OutRangeV)
      } 
    }else{
      InRange=OccSp
      plot(InRange$longitude,InRange$latitude,xlim=c(-6,10)
           ,ylim=c(41,51),main=ListSp[j])
    }
    DataClean=rbind(DataClean,InRange)
  }else{
    #stop("probleme SLpred")
    MissingSp=c(MissingSp,ListSp[j])
  }
}

fwrite(DataClean,paste0("DataForSINP",Sys.Date(),".csv"),sep=";")
