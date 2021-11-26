library(data.table)

ValidData=fread("ValidDataPart.csv")
#ValidData=fread("C:/wamp64/www/ovt_valid2012ALL.csv")
exportDir="./VigieChiro/Raw"
SpeciesList=fread("SpeciesList.csv")
RefFreqSp=fread("RefFreqSp123.csv")
DiscardSp=c("Chirosp","Ortsp.")
#EVTnow=fread("./Tadarida/rounds/compil_validation200728/export_validtot201103.txt")
EVTnow=fread("C:/wamp64/www/export_validtot210216.txt")

names(EVTnow)[10]="temps_fin"
EVTO=subset(EVTnow,EVTnow$obs.espece!="")
EVTV=subset(EVTnow,EVTnow$valid.espece!="")

#test=aggregate(ValidOU$donnee,by=list(ValidOU$donnee),length)
#tail(subset(test,test$x>3),30)
#test2020=subset(test,substr(test$Group.1,11,14)=="2019")
#tail(subset(test2020,test2020$x>2),30)

ValidData=ValidData[order(ValidData$observateur_probabilite,decreasing=T),]
ValidOU=subset(ValidData,select=c("participation","donnee"
                                  ,"observateur_taxon"
                                  ,"observateur_probabilite"))
ValidOU=unique(ValidOU)
ValidOU=subset(ValidOU,ValidOU$observateur_taxon!="")

test1=paste(ValidOU$donnee,ValidOU$observateur_taxon)
test2=paste(EVTV$donnee,EVTV$obs.espece)
test12=(test1 %in% test2)
ValidOU=subset(ValidOU,!test12)

ValidOU=ValidOU[order(ValidOU$observateur_probabilite,decreasing=T),]
ValidOU=unique(ValidOU,by=c("participation","donnee","observateur_taxon"))

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
    
    #VfO=subset(Vf,!(Vf$observateur_taxon %in% ef$obs.espece))
    VfO=subset(Vf,!(Vf$observateur_taxon %in% DiscardSp))
    if(nrow(VfO)>0)
    {
      testO=match(Vf$observateur_taxon,ef$espece)
      for (k in 1:length(testO))
      {
        #case 1: correspondance between species
        if(!is.na(testO[k]))
        {
          esel=ef[testO[k],]
          esel$obs.espece.new=VfO$observateur_taxon[k]
          esel$obs.proba.new=VfO$observateur_probabilite[k]
          Easymatch=rbind(Easymatch,esel)
          
        }
      }
      eA=subset(ef,!(ef$espece %in% Vf$observateur_taxon))
      #EVTa=subset(EVTf,!(EVTf$espece %in% Vf$observateur_taxon))
      VA=subset(Vf,!(Vf$observateur_taxon %in% ef$espece))
      #case 2: species errors
      if((nrow(VA)>0)&(nrow(eA)>0))
      {
      
      
      taxaV=vector()
      confV=vector()
      taxae=vector()
      Dist=vector()
      for (l in 1:nrow(VA))
      {
        isBatV=(SpeciesList$Group[match(VA$observateur_taxon[l]
                                        ,SpeciesList$Esp)]=="bat")
        FV=RefFreqSp$SpFmed[match(VA$observateur_taxon[l]
                                  ,RefFreqSp$Group.1)]
        for (m in 1:nrow(eA))
        {
          isBate=(SpeciesList$Group[match(eA$espece[m]
                                          ,SpeciesList$Esp)]=="bat")
          Fe=RefFreqSp$SpFmed[match(eA$espece[m]
                                    ,RefFreqSp$Group.1)]
          taxaV=c(taxaV,VA$observateur_taxon[l])
          confV=c(confV,VA$observateur_probabilite[l])
          taxae=c(taxae,eA$espece[m])
          Dist=c(Dist,abs(Fe-FV)+abs(isBate-isBatV)*10)
        }
        
      }
      DataDist=data.frame(cbind(taxaV,confV,taxae,Dist))
      DataDist=DataDist[order(DataDist$Dist),]
      #if(nrow(DataDist)>1){stop("dist test")}
      while(nrow(DataDist)>0)
      {
        esel=subset(eA,eA$espece==DataDist$taxae[1])
        esel$obs.espece.new=DataDist$taxaV[1]
        esel$obs.proba.new=DataDist$confV[1]
        Easymatch=rbind(Easymatch,esel)
        DataDist=subset(DataDist,DataDist$taxaV!=DataDist$taxaV[1])
        DataDist=subset(DataDist,DataDist$taxae!=DataDist$taxae[1])
        VA=subset(VA,VA$observateur_taxon!=DataDist$taxaV[1])
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
      esel$obs.espece.new=VA$observateur_taxon[o]
      esel$obs.proba.new=VA$observateur_probabilite[o]
      esel$valid.espece=NA
      esel$valid.proba=NA
      esel$obs.espece=NA
      esel$obs.proba=NA
      Easymatch=rbind(Easymatch,esel)
    }
  }
  
  #AvailableToNextStep=rbind(AvailableToNextStep,eA) 
} 

Easymatch$obs.proba.new[Easymatch$obs.proba.new==""]="POSSIBLE"


fwrite(Easymatch,"ValidOmatch.csv",sep=";")

ovt=subset(Easymatch,select=c("participation","donnee","temps_debut","temps_fin"
                              ,"frequence","espece"	,"probabilite",
                              "proprietaire"	,"obs.espece.new"
                              ,"obs.proba.new"
                              ,"valid.espece"	,"valid.proba","obs.espece"
                              ,"obs.proba"))
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
