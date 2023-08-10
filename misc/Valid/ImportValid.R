library(data.table)
library(mongolite)
library(data.table)
library(beepr)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(uuid)
library(jsonlite)
library(rjson)

ValidDir="C:/Users/yvesb/Documents/Tadarida/rounds/valid2101"
#exportDir="./VigieChiro/Raw"
SpeciesList=fread("C:/Users/yvesb/Documents/Tadarida/Tadarida-C/tadaridaC_src/other_inputs/SpeciesList.csv")
RefFreqSp=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/RefFreqSp123.csv")
DiscardSp=c("Chirosp","Ortsp.")
#EVTnow=fread("./Tadarida/rounds/compil_validation200728/export_validtot201103.txt")
EVTnow=fread("C:/Users/yvesb/Documents/www/export_validtot230301.txt")
mongo=fread("C:/Users/yvesb/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/mongos.txt"
            ,sep="$",h=F)
test=F #T si base de test, F si base de prod
ListProbaValid=c("POSSIBLE","PROBABLE","SUR","")


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}

participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
donnee_obs = mongo(collection="donnees", db="vigiechiro", url=connection_string)
taxa= mongo(collection="taxons", db="vigiechiro", url=connection_string)

Sys.time() #~1sec / 1e3 sites
alldatapart<-participations$find(fields='{}')
Sys.time() #~1sec / 1e3 sites
alldatataxa<-taxa$find(fields='{}')


names(EVTnow)[10]="temps_fin"
EVTO=subset(EVTnow,EVTnow$obs.espece!="")
EVTV=subset(EVTnow,EVTnow$valid.espece!="")

#test=aggregate(ValidVU$donnee,by=list(ValidVU$donnee),length)
#tail(subset(test,test$x>3),30)
#test2020=subset(test,substr(test$Group.1,11,14)=="2019")
#tail(subset(test2020,test2020$x>2),30)

ValidFiles=list.files(ValidDir,recursive = T,full.names = T)
ValidFiles=subset(ValidFiles,!grepl("/~\\$",ValidFiles))
print(paste(length(ValidFiles),"fichiers de validation"))
ValidFiles=subset(ValidFiles,!grepl("/Fait/",ValidFiles))
print(paste(length(ValidFiles),"fichiers de validation encore à traiter"))

for (w in 1:length(ValidFiles)){
  if(grepl(".csv",ValidFiles[w])){
    ValidData=fread(ValidFiles[w])
  }else{
    stop("code autres que csv")
  }
  #chercher référence participation dans nom de fichier
  if(grepl("participation-",basename(ValidFiles[w]))){
    RefPart0=tstrsplit(basename(ValidFiles[w]),split="participation-")[[2]]
    RefPart=gsub("-observations.csv","",RefPart0)
    if(nchar(RefPart)!=24){stop("id participation non conforme")}
  }else{
    stop("code sans ref participation")
    RefPart=NA
  }
  
  if(sum(!(ValidData$validateur_probabilite %in% ListProbaValid))>0){stop("pb code confiance")}
  
  
  ValidData$participation=RefPart
  
  if("nom du fichier" %in% names(ValidData)){
    ValidData$donnee=ValidData$`nom du fichier`
  }
  
  
  ValidData=ValidData[order(ValidData$validateur_probabilite,decreasing=T),]
  ValidVU=subset(ValidData,select=c("participation","donnee","tadarida_taxon"
                                    ,"validateur_taxon"
                                    ,"validateur_probabilite"))
  ValidVU=unique(ValidVU)
  ValidVU=subset(ValidVU,ValidVU$validateur_taxon!="")
  test=match(ValidVU$validateur_taxon,alldatataxa$libelle_court)
  if(sum(is.na(test))){stop("codes sp manquants")}
  
  ValidVU$donnee=gsub(".wav","",ValidVU$donnee)
  
  test1=paste(ValidVU$donnee,ValidVU$validateur_taxon)
  test2=paste(EVTV$donnee,EVTV$obs.espece)
  test12=(test1 %in% test2)
  table(test12)
  if(sum(test12)>0){stop("certaines validations déja intégrées")}
  ValidVU=subset(ValidVU,!test12)
  
  ValidVU=ValidVU[order(ValidVU$validateur_probabilite,decreasing=T),]
  ValidVU=unique(ValidVU,by=c("participation","donnee","validateur_taxon"))
  
  ListPart=unique(ValidVU$participation)
  # ParPref=substr(ValidVU$participation,1,3)
  # table(ParPref)
  # test=subset(ValidVU,ParPref=="")
  # if(nrow(test)>0){stop("participations manquantes")}
  
  Easymatch=data.frame()
  #AvailableToNextStep=data.frame()
  for (i in 1:length(ListPart))
  {
    print(paste(i,length(ListPart),Sys.time()))
    Vp=subset(ValidVU,ValidVU$participation==ListPart[i])
    Sys.time()
    Obsj<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',ListPart[i],'"}}'))
    #ep=fread(paste0(exportDir,"/export_",unique(ParPref)[i],".csv"))
    Sys.time()
    test=subset(Obsj,Obsj$titre=="Car111581-2023-Pass1-Z1-PidCarrirForg-0_20230804_223422_000")
    test$observations[[1]]
    
    ListFichier=unique(Vp$donnee)
    for (j in 1:length(ListFichier))
    {
      Vf=subset(Vp,Vp$donnee==ListFichier[j])
      ef=subset(Obsj,Obsj$titre==ListFichier[j])
      test117=match(ef$observations[[1]]$tadarida_taxon,alldatataxa$`_id`)
      print(paste0(alldatataxa$libelle_court[test117]," > ",Vf$validateur_taxon))
      print(ef$observations[[1]]$tadarida_probabilite)
      EVTf=subset(EVTO,EVTO$donnee==ListFichier[j])
      # if(!"temps_fin" %in% names(ef))
      # {
      #   names(ef)[10]="temps_fin"
      # }
      # 
      #VfO=subset(Vf,!(Vf$validateur_taxon %in% ef$valid.espece))
      VfO=subset(Vf,!(Vf$validateur_taxon %in% DiscardSp))
      if(nrow(VfO)>0)
      {
        testT0=match(VfO$tadarida_taxon,alldatataxa$libelle_court)
        testT=match(alldatataxa$`_id`[testT0],ef$observations[[1]]$tadarida_taxon)
        testOO=match(VfO$validateur_taxon,alldatataxa$libelle_court)
        testO=match(alldatataxa$`_id`[testOO],ef$observations[[1]]$tadarida_taxon)
        for (k in 1:length(testT))
        {
          #test=donnee_obs$export(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "',ListFichier[j],'"}'))
          # test=donnee_obs$export(query=paste0('{"participation":{"$oid":"',ListPart[i]
          #                                     ,'"},"titre" : "'
          #                                     ,"Car111581-2023-Pass1-Z1-PidCarrirForg-0_20230804_223422_000",'"}'))
          # 
          
          #case A - tadarida taxon exists
          if(!is.na(testT[k]))
          {
            ExpDataj=capture.output(donnee_obs$export(query=paste0('{"participation":{"$oid":"'
                                                                   ,ListPart[i],'"},"titre" : "'
                                                                   ,ListFichier[j],'"}')))
            
            #if(grepl("validateur",ExpDataj)){stop("validateur existe déjà")}
            
            testF=unlist(gregexpr('"frequence_mediane"', ExpDataj)) 
            testObs=unlist(gregexpr('"observations"', ExpDataj))
            testEndObs=unlist(gregexpr(']', ExpDataj))
            if(length(testObs)>1){"pb multiple obs"}
            if(length(testEndObs)>(nrow(ef$observations[[1]])+1)){"pb obs structure not clear"}
            
            
            
            # esel=ef[testT[k],]
            # ef$observations[[1]]$validateur_taxon[testT[k]]=alldatataxa$`_id`[testOO][k]
            # ef$observations[[1]]$validateur_probabilite[testT[k]]=VfO$validateur_probabilite[k]
            # 
            JsonObs=paste0('"validateur_probabilite" : "',VfO$validateur_probabilite[k]
                           ,'", "validateur_taxon" : { "$oid" : "',alldatataxa$`_id`[testOO][k]
                           ,'" },')
            
            JsonBefore=substr(ExpDataj,testObs+17,testF[k]-1)
            JsonAfter=substr(ExpDataj,testF[k],testEndObs[length(testEndObs)])
            JsonNewk=paste0(JsonBefore,JsonObs,JsonAfter)
            JsonNewk=gsub("\\]\\]"," ] ",JsonNewk)
            JsonNewk=gsub("\\[\\["," [ ",JsonNewk)
            
            
            testU=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                 ,ListFichier[j],'"}')
                                    ,paste0('{"$set":{"observations":',JsonNewk,'}}'))
            if(testU[[1]]!=1){stop("modification non prise en compte")}
            # esel$valid.proba.new=VfO$validateur_probabilite[k]
            # Easymatch=rbind(Easymatch,esel)
            
          }else{
            stop("code tadarida taxon absent")
          }
        }
        #integration dans la base
        
        
      }
    }
  }
}


#         
#         
#         
#         #case 1: correspondance between species
#         if(!is.na(testO[k]))
#         {
#           esel=ef[testO[k],]
#           esel$valid.espece.new=VfO$validateur_taxon[k]
#           esel$valid.proba.new=VfO$validateur_probabilite[k]
#           Easymatch=rbind(Easymatch,esel)
#           
#         }
#       }
#       eA=subset(ef,!(ef$espece %in% Vf$validateur_taxon))
#       EVTa=subset(EVTf,!(EVTf$espece %in% Vf$validateur_taxon))
#       VA=subset(Vf,!(Vf$validateur_taxon %in% ef$espece))
#       #case 2: species errors
#       if((nrow(VA)>0)&(nrow(eA)>0))
#       {
#         test1=paste(VA$donnee,VA$validateur_taxon)
#         test2=paste(EVTa$donnee,EVTa$obs.espece)
#         test12=match(test1,test2)
#         if(sum(!is.na(test12))>0)
#         {
#           #stop("case when corresp valid<>obs but not espece")
#           testnum=subset(test12,!is.na(test12))
#           esel=EVTa[testnum,]
#           #if(nrow(esel)>1){stop("ambiguity obs.espece")}
#           test2b=paste(esel$donnee,esel$obs.espece)
#           test21=match(test2b,test1)
#           for (z in 1:length(testnum))
#           {
#             esel$valid.espece.new[z]=VfO$validateur_taxon[test21[z]]
#             esel$valid.proba.new[z]=VfO$validateur_probabilite[test21[z]]
#             Easymatch=rbind(Easymatch,esel)
#           }
#           eA=subset(eA,!(eA$espece %in% esel$espece))
#           VA=subset(VA,!(VA$validateur_taxon %in% esel$valid.espece.new))
#         }
#         
#         
#       }
#       
#     }
#     if((nrow(VA)>0)&(nrow(eA)>0))
#     {
#       
#       taxaV=vector()
#       confV=vector()
#       taxae=vector()
#       Dist=vector()
#       for (l in 1:nrow(VA))
#       {
#         isBatV=(SpeciesList$Group[match(VA$validateur_taxon[l]
#                                         ,SpeciesList$Esp)]=="bat")
#         FV=RefFreqSp$SpFmed[match(VA$validateur_taxon[l]
#                                   ,RefFreqSp$Group.1)]
#         for (m in 1:nrow(eA))
#         {
#           isBate=(SpeciesList$Group[match(eA$espece[m]
#                                           ,SpeciesList$Esp)]=="bat")
#           Fe=RefFreqSp$SpFmed[match(eA$espece[m]
#                                     ,RefFreqSp$Group.1)]
#           taxaV=c(taxaV,VA$validateur_taxon[l])
#           confV=c(confV,VA$validateur_probabilite[l])
#           taxae=c(taxae,eA$espece[m])
#           Dist=c(Dist,abs(Fe-FV)+abs(isBate-isBatV)*10)
#         }
#         
#       }
#       DataDist=data.frame(cbind(taxaV,confV,taxae,Dist))
#       DataDist=DataDist[order(DataDist$Dist),]
#       #if(nrow(DataDist)>1){stop("dist test")}
#       while(nrow(DataDist)>0)
#       {
#         esel=subset(eA,eA$espece==DataDist$taxae[1])
#         esel$valid.espece.new=DataDist$taxaV[1]
#         esel$valid.proba.new=DataDist$confV[1]
#         Easymatch=rbind(Easymatch,esel)
#         DataDist=subset(DataDist,DataDist$taxaV!=DataDist$taxaV[1])
#         DataDist=subset(DataDist,DataDist$taxae!=DataDist$taxae[1])
#         VA=subset(VA,VA$validateur_taxon!=DataDist$taxaV[1])
#       }
#     }
#     
#     #case 3: no data available > creating "empty" lines  
#     if(nrow(VA)>0)
#     {
#       #stop("test empty")
#       for (o in 1:nrow(VA))
#       {
#         esel=Easymatch[1,]
#         esel$participation=Vf$participation[1]
#         esel$donnee=Vf$donnee[1]
#         esel$date=ef$date[1]
#         esel$frequence=0
#         esel$espece="empty"
#         esel$probabilite=0
#         esel$proprietaire=ef$proprietaire[1]
#         esel$email=ef$email[1]
#         esel$temps_debut=0
#         esel$temps_fin=0
#         esel$valid.espece.new=VA$validateur_taxon[o]
#         esel$valid.proba.new=VA$validateur_probabilite[o]
#         esel$valid.espece=NA
#         esel$valid.proba=NA
#         esel$obs.espece=NA
#         esel$obs.proba=NA
#         Easymatch=rbind(Easymatch,esel)
#       }
#     }
#   }
#   #AvailableToNextStep=rbind(AvailableToNextStep,eA) 
# } 
# 
# Easymatch$valid.proba.new[Easymatch$valid.proba.new==""]="POSSIBLE"
# 
# t1=paste(Easymatch$donnee,Easymatch$espece)
# t2=paste(EVTO$donnee,EVTO$espece)
# t12=match(t1,t2)
# #table(t12)
# 
# for (h in 1:nrow(Easymatch))
# {
#   if(!is.na(t12[h]))
#   {
#     #stop("test")
#     Easymatch$obs.espece[h]=EVTO$obs.espece[t12[h]]
#     Easymatch$obs.proba[h]=EVTO$obs.proba[t12[h]]
#   }
# }
# 
# Easymatch$obs.espece.new=Easymatch$obs.espece
# Easymatch$obs.proba.new=Easymatch$obs.proba
# 
# fwrite(Easymatch,"ValidVmatch.csv",sep=";")
# 
# ovt=subset(Easymatch,select=c("participation","donnee","temps_debut","temps_fin"
#                               ,"frequence","espece"	,"probabilite",
#                               "proprietaire"	,"obs.espece.new"
#                               ,"obs.proba.new","valid.espece.new"
#                               ,"valid.proba.new","obs.espece"
#                               ,"obs.proba"
#                               ,"valid.espece"	,"valid.proba"))
# ovt$proprietaire=""
# ovt$obs.espece[is.na(ovt$obs.espece)]=""
# ovt$obs.espece.new[is.na(ovt$obs.espece.new)]=""
# ovt[is.na(ovt)]=""
# ovt=subset(ovt,ovt$valid.espece=="")
# table(ovt$valid.proba,ovt$espece)
# summary(ovt$espece=="")
# table(substr(ovt$donnee,1,3))
# 
# 
# ovtu=unique(ovt,by=c("participation","donnee","espece","valid.espece.new"))
# ovt=ovtu
# 
# isBatV=(SpeciesList$Group[match(ovt$valid.espece.new
#                                 ,SpeciesList$Esp)]=="bat")
# FV=RefFreqSp$SpFmed[match(ovt$valid.espece.new
#                           ,RefFreqSp$Group.1)]
# FV[is.na(FV)]=40
# isBate=(SpeciesList$Group[match(ovt$espece
#                                 ,SpeciesList$Esp)]=="bat")
# table(subset(ovt$espece,is.na(isBate)))
# isBate[is.na(isBate)]=F
# Fe=RefFreqSp$SpFmed[match(ovt$espece
#                           ,RefFreqSp$Group.1)]
# Fe[is.na(Fe)]=40
# 
# Dist=abs(Fe-FV)+abs(isBate-isBatV)*10
# ovt=ovt[order(Dist),]
# ovt=ovt[order(ovt$espece),]
# testE=(ovt$espece=="empty")
# ovt=ovt[order(testE),]
# ovt=ovt[order(ovt$donnee),]
# ovt=ovt[order(ovt$participation),]
# 
# 
# 
# fwrite(ovt,"ovt_vvalid.csv",sep=";")
# }
