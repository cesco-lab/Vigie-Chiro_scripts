library(data.table)

ValidData=fread("C:/wamp64/www/ovt_valid2012ALL.csv")
exportDir="./VigieChiro/Raw"
SpeciesList=fread("SpeciesList.csv")
RefFreqSp=fread("RefFreqSp123.csv")
DiscardSp=c("Chirosp","Ortsp.")
#EVTnow=fread("./Tadarida/rounds/compil_validation200728/export_validtot201103.txt")

#test=aggregate(ValidOU$donnee,by=list(ValidOU$donnee),length)
#tail(subset(test,test$x>3),30)
#test2020=subset(test,substr(test$Group.1,11,14)=="2019")
#tail(subset(test2020,test2020$x>2),30)

ValidData=ValidData[order(ValidData$obs.proba.new,decreasing=T),]
ValidData=ValidData[order(ValidData$valid.proba.new,decreasing=T),]
NotToMatch=subset(ValidData,ValidData$espece!="")
ToMatch=subset(ValidData,ValidData$espece=="")
ValidOU=subset(ToMatch,select=c("participation","donnee","espece"
                                  ,"obs.espece.new"
                                  ,"obs.proba.new","valid.espece.new"
                                  ,"valid.proba.new"))
ValidOU=unique(ValidOU)
ValidOU$especeRef=ifelse(ValidOU$valid.espece.new==""
                         ,ValidOU$obs.espece.new
                         ,ValidOU$valid.espece.new)

test=match(ValidOU$especeRef,SpeciesList$Esp)
ValidOU$especeRef=SpeciesList$Nesp2[test]

ValidOU=ValidOU[order(ValidOU$obs.proba.new,decreasing=T),]
ValidOU$donnee=gsub(".wav","",ValidOU$donnee)

ParPref=substr(ValidOU$participation,1,3)
table(ParPref)
test=subset(ValidOU,ParPref=="")


Easymatch=data.frame()
#AvailableToNextStep=data.frame()
for (i in 1:length(unique(ParPref)))
{
  print(paste(i,length(unique(ParPref)),Sys.time()))
  Vp=subset(ValidOU,ParPref==unique(ParPref)[i])
  ep=fread(paste0(exportDir,"/export_",unique(ParPref)[i],".csv"))
  ListFichier=unique(Vp$donnee)
    for (j in 1:length(ListFichier))
  {
    Vf=subset(Vp,Vp$donnee==ListFichier[j])
    ef=subset(ep,ep$donnee==ListFichier[j])
    #EVTf=subset(EVTO,EVTO$donnee==ListFichier[j])
    if(!"temps_fin" %in% names(ef))
    {
      names(ef)[10]="temps_fin"
    }
    
    #VfO=subset(Vf,!(Vf$obs.espece.new %in% ef$obs.espece))
    VfO=subset(Vf,!(Vf$especeRef %in% DiscardSp))
    if(nrow(VfO)>0)
    {
      testO=match(Vf$especeRef,ef$espece)
      for (k in 1:length(testO))
      {
        #case 1: correspondance between species
        if(!is.na(testO[k]))
        {
          esel=ef[testO[k],]
          esel$obs.espece.new=VfO$obs.espece.new[k]
          esel$obs.proba.new=VfO$obs.proba.new[k]
          esel$valid.espece.new=VfO$valid.espece.new[k]
          esel$valid.proba.new=VfO$valid.proba.new[k]
          Easymatch=rbind(Easymatch,esel)
          
        }
      }
      eA=subset(ef,!(ef$espece %in% Vf$especeRef))
      #EVTa=subset(EVTf,!(EVTf$espece %in% Vf$obs.espece.new))
      VA=subset(Vf,!(Vf$especeRef %in% ef$espece))
      #case 2: species errors
      if((nrow(VA)>0)&(nrow(eA)>0))
      {
      
      
      taxaV=vector()
      confV=vector()
      taxaV2=vector()
      confV2=vector()
      taxae=vector()
      Dist=vector()
      for (l in 1:nrow(VA))
      {
        isBatV=(SpeciesList$Group[match(VA$especeRef[l]
                                        ,SpeciesList$Esp)]=="bat")
        FV=RefFreqSp$SpFmed[match(VA$especeRef[l]
                                  ,RefFreqSp$Group.1)]
        for (m in 1:nrow(eA))
        {
          isBate=(SpeciesList$Group[match(eA$espece[m]
                                          ,SpeciesList$Esp)]=="bat")
          Fe=RefFreqSp$SpFmed[match(eA$espece[m]
                                    ,RefFreqSp$Group.1)]
          taxaV=c(taxaV,VA$obs.espece.new[l])
          confV=c(confV,VA$obs.proba.new[l])
          taxaV2=c(taxaV2,VA$valid.espece.new[l])
          confV2=c(confV2,VA$valid.proba.new[l])
          
          #stop("case 108")
          taxae=c(taxae,eA$espece[m])
          Dist=c(Dist,abs(Fe-FV)+abs(isBate-isBatV)*10)
        }
        
      }
      DataDist=data.frame(cbind(taxaV,confV,taxaV2,confV2,taxae,Dist))
      DataDist=DataDist[order(DataDist$Dist),]
      #if(nrow(DataDist)>1){stop("dist test")}
      while(nrow(DataDist)>0)
      {
        esel=subset(eA,eA$espece==DataDist$taxae[1])
        esel$obs.espece.new=DataDist$taxaV[1]
        esel$obs.proba.new=DataDist$confV[1]
        esel$valid.espece.new=DataDist$taxaV2[1]
        esel$valid.proba.new=DataDist$confV2[1]
        
        Easymatch=rbind(Easymatch,esel)
        spRefDD=ifelse(DataDist$taxaV2=="",as.character(DataDist$taxaV)
                       ,as.character(DataDist$taxaV2))
        DataDist=subset(DataDist,spRefDD!=spRefDD[1])
        DataDist=subset(DataDist,DataDist$taxae!=spRefDD[1])
        VA=subset(VA,VA$especeRef!=spRefDD[1])
      }
    }
  }
  #case 3: no data available > creating "empty" lines  
  if(nrow(VA)>0)
  {
    #stop("test empty")
    for (o in 1:nrow(VA))
    {
      esel=Easymatch[1,]
      esel$participation=Vf$participation[1]
      esel$donnee=Vf$donnee[1]
      esel$date=ef$date[1]
      esel$frequence=0
      esel$espece="empty"
      esel$probabilite=0
      esel$proprietaire=ef$proprietaire[1]
      esel$email=ef$email[1]
      esel$temps_debut=0
      esel$temps_fin=0
      esel$obs.espece.new=VA$obs.espece.new[o]
      esel$obs.proba.new=VA$obs.proba.new[o]
      esel$valid.espece.new=VA$valid.espece.new[o]
      esel$valid.proba.new=VA$valid.proba.new[o]
      esel$valid.espece=NA
      esel$valid.proba=NA
      esel$obs.espece=NA
      esel$obs.proba=NA
      Easymatch=rbind(Easymatch,esel)
    }
  }
  
  #AvailableToNextStep=rbind(AvailableToNextStep,eA) 
} 
}
Easymatch$obs.proba.new[Easymatch$obs.proba.new==""]="POSSIBLE"


fwrite(Easymatch,"ValidOmatch.csv",sep=";")

ovt=subset(Easymatch,select=c("participation","donnee","temps_debut","temps_fin"
                              ,"frequence","espece"	,"probabilite",
                              "proprietaire"	,"obs.espece.new"
                              ,"obs.proba.new","valid.espece.new"
                              ,"valid.proba.new"
                              ,"obs.espece"
                              ,"obs.proba"
                              ,"valid.espece"	,"valid.proba"))
ovt$proprietaire=""
ovt$obs.espece[is.na(ovt$obs.espece)]=""
ovt=subset(ovt,ovt$obs.espece=="")
ovt[is.na(ovt)]=""
table(ovt$obs.proba,ovt$espece)
summary(ovt$espece=="")
table(substr(ovt$donnee,1,3))

isBatV=(SpeciesList$Group[match(ovt$obs.espece.new
                                ,SpeciesList$Esp)]=="bat")
FV=RefFreqSp$SpFmed[match(ovt$obs.espece.new
                          ,RefFreqSp$Group.1)]
FV[is.na(FV)]=40

isBate=(SpeciesList$Group[match(ovt$espece
                                ,SpeciesList$Esp)]=="bat")
Fe=RefFreqSp$SpFmed[match(ovt$espece
                          ,RefFreqSp$Group.1)]
Fe[is.na(Fe)]=40

Dist=abs(Fe-FV)+abs(isBate-isBatV)*10
ovt=ovt[order(Dist),]
ovt=ovt[order(ovt$espece),]
testE=(ovt$espece=="empty")
ovt=ovt[order(testE),]
ovt=ovt[order(ovt$donnee),]
ovt=ovt[order(ovt$participation),]


ovt$ne=ovt$espece
for (z in 2:nrow(ovt))
{
  if((ovt$espece[z]==ovt$espece[z-1])&
     (ovt$donnee[z]==ovt$donnee[z-1])&
     (ovt$participation[z]==ovt$participation[z-1]))
  {
    #ovt[(z-1):z,]
    #stop()
    ovt$ne[z]="empty"
    ovt$probabilite[z]=0
    ovt$temps_debut[z]=0
    ovt$temps_fin[z]=0
    ovt$frequence[z]=0
  }
}
#table(subset(ovt$espece,ovt$ne=="empty"))
ovt$espece=ovt$ne
ovt$ne=NULL

ovtu=unique(ovt,by=c("participation","donnee","espece","obs.espece.new"))
ovt=ovtu
fwrite(ovt,"ovt_obsvalid.csv",sep=";")
