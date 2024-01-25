library(data.table)
library(readxl)
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
library(lubridate)

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
ListProbaValid=c("POSSIBLE","PROBABLE","SUR","",NA)
ToBeCorrected=data.frame(Error=c("myogt","Pipkuhl"),Correction=c("MyoGT","Pipkuh"))
CreationLigneVide=", { \"tadarida_taxon\" : { \"$oid\" : \"5a9806e668062c000f9722ad\" }, \"frequence_mediane\" : 0, \"tadarida_probabilite\" : 0.0, \"temps_debut\" : 0, \"temps_fin\" : 0.0,"

AutomaticCorrection=subset(alldatataxa$libelle_court,substr(alldatataxa$libelle_court,1,1)==
                             toupper(substr(alldatataxa$libelle_court,1,1)))
AutomaticError=tolower(AutomaticCorrection)
AutomaticCorrections=data.frame(Error=AutomaticError,Correction=AutomaticCorrection)
CompleteTaxaCorrections=rbind(ToBeCorrected,AutomaticCorrections)


if(test){
  connection_string=mongo$V1[2]
}else{
  connection_string=mongo$V1[1]
}

sites = mongo(collection="sites", db="vigiechiro", url=connection_string)
participations = mongo(collection="participations", db="vigiechiro", url=connection_string)
donnee_obs = mongo(collection="donnees", db="vigiechiro", url=connection_string)
taxa= mongo(collection="taxons", db="vigiechiro", url=connection_string)

Sys.time()
alldatasites <- sites$find(query=paste0('{"protocole" : {"$oid":"54bd090f1d41c8103bad6252"}}'),fields='{}') #protocole PF
Sys.time() #~1sec / 1e3 sites
alldatapart<-participations$find(fields='{}')
Sys.time() #~1sec / 1e3 sites
alldatataxa<-taxa$find(fields='{}')




DirDone=paste0(ValidDir,"/Fait/")
dir.create(DirDone)

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

for (w in 46:length(ValidFiles)){
  print(w)
  print(ValidFiles[w])
  if(grepl(".csv",ValidFiles[w])){
    ValidData=fread(ValidFiles[w])
  }else{
    if(grepl(".xlsx",ValidFiles[w])){
      ValidData=read_xlsx(ValidFiles[w])
    }else{
      stop("code autres que csv ou xlsx")
      
    }
  }
  
  #gestion problème des virgules dans tadarida_taxon_autres prises pour des separateurs
  # if("observateur_taxon" %in% names(ValidData)){
  #   test91=match("observateur_taxon", names(ValidData))
  #   
  #   for (b in 1:nrow(ValidData)){
  #     if(substr(as.data.frame(ValidData)[b,test91],1,1)==" "){
  #       if(b%)
  #       stop("coder correct")
  #     }
  #     
  #     
  #   }
  #   }
  # 
  
  if("nom du fichier" %in% names(ValidData)){
    ValidData$donnee=ValidData$`nom du fichier`
  }
  
  #remplir les données si manquantes
  if(sum(ValidData$donnee=="")>0){stop("code donnee manquante")}
  if(sum(is.na(ValidData$donnee))>0){stop("code donnee manquante2")}
  
  
  #chercher référence participation dans nom de fichier
  if(grepl("participation-5",basename(ValidFiles[w]))|
     grepl("participation-6",basename(ValidFiles[w]))){
    RefPart0=tstrsplit(basename(ValidFiles[w]),split="participation-")[[2]]
    #RefPart=gsub("-observations.csv","",RefPart0)
    RefPart=tstrsplit(RefPart0,split="-observations")[[1]]
    RefPart=gsub(".csv","",RefPart)
    if(nchar(RefPart)!=24){stop("id participation non conforme")}
  }else{
    #stop("code sans ref participation")
    if("participation" %in% names(ValidData)){
      #stop("coder participation dans le fichier input")
      print("L104")
      RefPart<-ValidData$participation
      print(length(RefPart))
    }else{
      print("L106")
      RefPart=vector()
      for (n in 1:nrow(ValidData))
      {
        NumCi=substr(basename(ValidData$donnee[n]),4,9)  
        print(NumCi)
        sitesi=subset(alldatasites
                      ,alldatasites$titre==paste0("Vigiechiro - Point Fixe-"
                                                  ,NumCi))
        if(nrow(sitesi)!=1){
          #stop("pb site")
          RefPart=c(RefPart,NA)
        }else{
          parti=subset(alldatapart,alldatapart$site==sitesi$'_id')
          Infoi=tstrsplit(basename(ValidData$donnee[n]),split="_")
          Datei=ymd(substr(Infoi[[length(Infoi)-2]]
                           ,nchar(Infoi[[length(Infoi)-2]])-7
                           ,nchar(Infoi[[length(Infoi)-2]])))
          print(Datei)
          # if(is.na (Datei)){
          #   Infoi2=tstrsplit(Infoi[[length(Infoi)-2]],split="-")  
          #   Datei=ymd(  
          # }
          partisel=subset(parti,as.Date(parti$date_debut)<=Datei)
          partisel=subset(partisel,as.Date(partisel$date_fin)>=Datei)
          
          if(nrow(partisel)==0){
            #stop("participation manquante")
            RefPart=c(RefPart,NA)
          }else{
            
            Pointi=tstrsplit(basename(ValidData$donnee[n]),split="-")[[4]]
            print(Pointi)
            partisel2=subset(partisel,partisel$point==Pointi)
            
            if(nrow(partisel2)>1){
              #stop("pb participation")
              partisel2=subset(partisel,!is.na(partisel$traitement$etat))
            }
            
            if(nrow(partisel2)!=1){
              stop("pb participation")}
            RefPart=c(RefPart,partisel2$'_id')
            
          }
        }
      }
      # else{
      #   RefPart=NA
      # }
    }
  }
  if(!("validateur_probabilite" %in% names(ValidData))){
    if("validateur.proba" %in% names(ValidData)){
      ValidData$validateur_probabilite=ValidData$valid.proba
      ValidData$validateur_taxon=ValidData$valid.espece
      ValidData$tadarida_taxon=ValidData$espece
    }else{
      if("validateur_proba" %in% names(ValidData)){
        ValidData$validateur_probabilite=ValidData$validateur_proba
        ValidData$tadarida_taxon=ValidData$espece
        
      }else{
        stop("entetes non conformes")
      }
    }
    
  }
  ValidData$validateur_probabilite=toupper(ValidData$validateur_probabilite)
  print(table(ValidData$validateur_probabilite))
  if(sum(!(ValidData$validateur_probabilite %in% ListProbaValid))>0){stop("pb code confiance 195")}
  
  
  if(!("observateur_probabilite" %in% names(ValidData))){
    if("obs.proba" %in% names(ValidData)){
      ValidData$observateur_probabilite=ValidData$obs.proba
      ValidData$observateur_taxon=ValidData$obs.espece
    }else{
      if(sum(grepl("obs",names(ValidData)))>1){
        stop("entetes non conformes")
      }else{
        ValidData$observateur_probabilite=""
        ValidData$observateur_taxon=""
      }
    }
  }
  ValidData$observateur_probabilite=toupper(ValidData$observateur_probabilite)
  
  if(sum(!(ValidData$observateur_probabilite %in% ListProbaValid))>0){stop("pb code confiance")}
  table(ValidData$observateur_probabilite)
  
  ValidData$tadarida_taxon=gsub("\U00A0","",ValidData$tadarida_taxon)  
  ValidData$tadarida_taxon=gsub(" ","",ValidData$tadarida_taxon)
  ValidData$observateur_taxon=gsub("\U00A0","",ValidData$observateur_taxon)  
  ValidData$observateur_taxon=gsub(" ","",ValidData$observateur_taxon)
  ValidData$validateur_taxon=gsub("\U00A0","",ValidData$validateur_taxon)  
  ValidData$validateur_taxon=gsub(" ","",ValidData$validateur_taxon)
  
  ValidData$participation=RefPart
  #ValidData=subset(ValidData,!is.na(ValidData$participation))
  if(nrow(ValidData)>1){
    for (g in 2:nrow(ValidData)){
      if(is.na(ValidData$participation[g]))
      {
        ValidData$participation[g]=ValidData$participation[g-1]
      }
      
    }
  }else{
    stop("check bizarrerie 1 seule donnée isolée")
  }
  
  if(sum(is.na(ValidData$donnee)>0)){stop("pb saisie donnee")}
  # ValidOU=subset(ValidData,select=c("participation","donnee","tadarida_taxon"
  #                                   ,"observateur_taxon"
  #                                   ,"observateur_probabilite"))
  # ValidOU=unique(ValidOU)
  # ValidOU=subset(ValidOU,ValidOU$observateur_taxon!="")
  # test=match(ValidOU$observateur_taxon,alldatataxa$libelle_court)
  # if(nrow(ValidOU)>0){stop("code obs")}
  # if(sum(is.na(test))){stop("codes sp manquants")}
  # 
  # 
  # ValidData=ValidData[order(ValidData$validateur_probabilite,decreasing=T),]
  # ValidVU=subset(ValidData,select=c("participation","donnee","tadarida_taxon"
  #                                   ,"validateur_taxon"
  #                                   ,"validateur_probabilite"))
  # ValidVU=unique(ValidVU)
  # ValidVU=subset(ValidVU,ValidVU$validateur_taxon!="")
  # test=match(ValidVU$validateur_taxon,alldatataxa$libelle_court)
  # Missing=subset(ValidVU$validateur_taxon,is.na(test))
  # if(sum(is.na(test))){stop("codes sp manquants")}
  # 
  # ValidVU$donnee=gsub(".wav","",ValidVU$donnee)
  # 
  # test1=paste(ValidVU$donnee,ValidVU$validateur_taxon)
  # test2=paste(EVTV$donnee,EVTV$obs.espece)
  # test12=(test1 %in% test2)
  # table(test12)
  # if(sum(test12)>0){stop("certaines validations déja intégrées")}
  # ValidVU=subset(ValidVU,!test12)
  # 
  # ValidVU=ValidVU[order(ValidVU$validateur_probabilite,decreasing=T),]
  # ValidVU=unique(ValidVU,by=c("participation","donnee","tadarida_taxon","validateur_taxon"))
  
  ValidData=ValidData[order(ValidData$validateur_probabilite,decreasing=T),]
  ValidVOU=subset(ValidData,select=c("participation","donnee","tadarida_taxon"
                                     ,"observateur_taxon","observateur_probabilite"
                                     ,"validateur_taxon"
                                     ,"validateur_probabilite"))
  ValidVOU=unique(ValidVOU)
  ValidVOU$observateur_taxon[is.na(ValidVOU$observateur_taxon)]=""
  ValidVOU$observateur_probabilite[is.na(ValidVOU$observateur_taxon)]=""
  ValidVOU$validateur_taxon[is.na(ValidVOU$validateur_taxon)]=""
  ValidVOU$validateur_probabilite[is.na(ValidVOU$validateur_taxon)]=""
  
  ValidVOU=subset(ValidVOU,(ValidVOU$validateur_taxon!="")|(ValidVOU$observateur_taxon!=""))
  ValidVOU$validateur_taxon=ifelse(ValidVOU$validateur_taxon!="",
                                   tstrsplit(ValidVOU$validateur_taxon,split="\\+")[[1]],"")
  ValidVOU$observateur_taxon=ifelse(ValidVOU$observateur_taxon!="",
                                    tstrsplit(ValidVOU$observateur_taxon,split="\\+")[[1]],"")
  
  
  test=match(ValidVOU$validateur_taxon,c(alldatataxa$libelle_court,""))
  Missing=subset(ValidVOU$validateur_taxon,is.na(test))
  table(ValidVOU$validateur_taxon)[order(table(ValidVOU$validateur_taxon))]
  if(sum(is.na(test))){stop("codes sp manquants")}
  
  
  
  testCorr=match(ValidVOU$observateur_taxon,CompleteTaxaCorrections$Error)
  ValidVOU$observateur_taxon=ifelse(!is.na(testCorr),CompleteTaxaCorrections$Correction[testCorr]
                                    ,ValidVOU$observateur_taxon)
  
  test=match(ValidVOU$observateur_taxon,c(alldatataxa$libelle_court,""))
  Missing=subset(ValidVOU$observateur_taxon,is.na(test))
  table(ValidVOU$observateur_taxon)
  if(sum(is.na(test))){  
    print(table(Missing))
    stop("codes sp manquants")
  }
  
  
  
  # ValidData$tadarida_taxon=gsub(substr(ValidData$tadarida_taxon[1],7,7),""
  #                               ,ValidData$tadarida_taxon)  
  
  ValidVOU$donnee=gsub(".wav","",ValidVOU$donnee)
  
  test1=paste(ValidVOU$donnee,ValidVOU$validateur_taxon)
  test2=paste(EVTV$donnee,EVTV$obs.espece)
  test12=(test1 %in% test2)
  table(test12)
  #if(sum(test12)>0){stop("certaines validations déja intégrées")}
  ValidVOU=subset(ValidVOU,!test12)
  
  ValidVOU=ValidVOU[order(ValidVOU$validateur_probabilite,decreasing=T),]
  ValidVOU=unique(ValidVOU,by=c("participation","donnee","tadarida_taxon"
                                ,"observateur_taxon","validateur_taxon"))
  
  test265=((is.na(ValidVOU$observateur_probabilite))&(is.na(ValidVOU$validateur_probabilite)))
  if(sum(test265)>0){
    ValidVOU$observateur_probabilite=ifelse(ValidVOU$observateur_taxon==""
                                            ,"",ifelse(test265
                                                       ,"PROBABLE"
                                                       ,ValidVOU$observateur_probabilite))
    
    
    ValidVOU$validateur_probabilite=ifelse(ValidVOU$validateur_taxon==""
                                           ,"",ifelse(test265
                                                      ,"PROBABLE"
                                                      ,ValidVOU$validateur_probabilite))
    
    #stop("saisie confiance manquante")
  }
  
  
  ListPart=unique(ValidVOU$participation)
  if(length(ListPart)==0){
    if((mean(is.na(ValidData$observateur_taxon))==1)&
       (mean(is.na(ValidData$validateur_taxon))==1)){
      print("pas de données a integrer")
    }else{
      stop("pb ListPart") 
    }
    
  }else{
    # ParPref=substr(ValidVU$participation,1,3)
    # table(ParPref)
    # test=subset(ValidVU,ParPref=="")
    # if(nrow(test)>0){stop("participations manquantes")}
    
    #Easymatch=data.frame()
    #AvailableToNextStep=data.frame()
    for (i in 1:length(ListPart))
    {
      print(paste("i",i,length(ListPart),Sys.time()))
      Vp=subset(ValidVOU,ValidVOU$participation==ListPart[i])
      Sys.time()
      Obsj<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',ListPart[i],'"}}'))
      #ep=fread(paste0(exportDir,"/export_",unique(ParPref)[i],".csv"))
      Sys.time()
      #test=subset(Obsj,Obsj$titre=="Car111581-2023-Pass1-Z1-PidCarrirForg-0_20230804_223422_000")
      #test$observations[[1]]
      
      ListFichier=unique(Vp$donnee)
      for (j in 1:length(ListFichier))
      {
        print(paste("j",j,length(ListFichier)))
        Vf=subset(Vp,Vp$donnee==ListFichier[j])
        ef=subset(Obsj,Obsj$titre==ListFichier[j])
        if("tadarida_taxon" %in% names(ef$observations[[1]])){
          if(("validateur_taxon" %in% names(ef$observations[[1]]))|#{stop("code competition avec validation antérieure")}
             ("observateur_taxon" %in% names(ef$observations[[1]]))){
            #stop("code competition avec validation antérieure")
            Competition=T
          }else{
            Competition=F
          }
          
          test117=match(ef$observations[[1]]$tadarida_taxon,alldatataxa$`_id`)
          print(alldatataxa$libelle_court[test117])
          print(Vf$observateur_taxon)
          print(Vf$observateur_probabilite)
          print(Vf$validateur_taxon)
          print(Vf$validateur_probabilite)
          # print(paste0(alldatataxa$libelle_court[test117]," > ",Vf$observateur_taxon))
          # print(paste0(alldatataxa$libelle_court[test117]," > ",Vf$validateur_taxon))
          # print(ef$observations[[1]]$tadarida_probabilite)
          EVTf=subset(EVTO,EVTO$donnee==ListFichier[j])
          #if(nrow(EVTf)>0){stop("competition validation")}
          # if(!"temps_fin" %in% names(ef))
          # {
          #   names(ef)[10]="temps_fin"
          # }
          # 
          #VfO=subset(Vf,!(Vf$validateur_taxon %in% ef$valid.espece))
          #VfO=subset(Vf,!(Vf$validateur_taxon %in% DiscardSp))
          if(sum((Vf$validateur_taxon %in% DiscardSp))>0){stop("check DiscardSp")}
          VfO=Vf
          if(nrow(VfO)>0)
          {
            
            testT0=match(VfO$tadarida_taxon,alldatataxa$libelle_court)
            testT=match(alldatataxa$`_id`[testT0],ef$observations[[1]]$tadarida_taxon)
            testOO1=match(VfO$observateur_taxon,alldatataxa$libelle_court)
            testO1=match(alldatataxa$`_id`[testOO1],ef$observations[[1]]$tadarida_taxon)
            testOO=match(VfO$validateur_taxon,alldatataxa$libelle_court)
            testO=match(alldatataxa$`_id`[testOO],ef$observations[[1]]$tadarida_taxon)
            for (k in 1:length(testT))
            {
              print(paste("k",k,length(testT)))
              #test=donnee_obs$export(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "',ListFichier[j],'"}'))
              # test=donnee_obs$export(query=paste0('{"participation":{"$oid":"',ListPart[i]
              #                                     ,'"},"titre" : "'
              #                                     ,"Car111581-2023-Pass1-Z1-PidCarrirForg-0_20230804_223422_000",'"}'))
              # 
              
              ExpDataj=capture.output(donnee_obs$export(query=paste0('{"participation":{"$oid":"'
                                                                     ,ListPart[i],'"},"titre" : "'
                                                                     ,ListFichier[j],'"}')))
              
              #if(grepl("validateur",ExpDataj)){stop("validateur existe déjà")}
              
              testF=unlist(gregexpr('"frequence_mediane"', ExpDataj)) 
              testObs=unlist(gregexpr('"observations"', ExpDataj))
              testEndObs=unlist(gregexpr(']', ExpDataj))
              if(length(testObs)>1){
                print(ListPart[i])
                print(ListFichier[j])
                stop("pb multiple obs")
              }
              if(length(testEndObs)>(nrow(ef$observations[[1]])+1)){stop("pb obs structure not clear")}
              
              #case A - tadarida taxon exists
              if(!is.na(testT[k]))
              {
                
                if(Competition){
                  if("observateur_taxon" %in% names(ef$observations[[1]])){
                    if(alldatataxa$`_id`[testOO1[k]] %in% ef$observations[[1]]$observateur_taxon)
                    {
                      Skip1=T
                    }else{
                      print(ListPart[i])
                      print(ListFichier[j])
                      stop("vraie competition")
                      
                      
                    }
                  }else{
                    Skip1=F
                  }
                  if("validateur_taxon" %in% names(ef$observations[[1]])){
                    # if(alldatataxa$`_id`[testOO[k]] %in% ef$observations[[1]]$validateur_taxon)
                    # {
                    #   Skip2=T
                    # }else{
                    ek=subset(ef$observations[[1]],ef$observations[[1]]$tadarida_taxon==alldatataxa$`_id`[testT0[k]])
                    if(nrow(ek)>0){
                      if(is.na(ek$validateur_taxon[1]))
                      {
                        Skip2=F
                      }else{
                        if(ek$validateur_taxon[1]==alldatataxa$`_id`[testOO[k]]){
                          Skip2=T
                        }else{
                          print(ListPart[i])
                          print(ListFichier[j])
                          stop("vraie competition")
                        }
                      }
                    }else{
                      print(ListPart[i])
                      print(ListFichier[j])
                      stop("vraie competition")
                      
                    }
                    
                  }else{
                    Skip2=F
                  }
                  
                  Skip=(Skip1|Skip2)
                }else{
                  Skip=F
                }
                # esel=ef[testT[k],]
                # ef$observations[[1]]$validateur_taxon[testT[k]]=alldatataxa$`_id`[testOO][k]
                # ef$observations[[1]]$validateur_probabilite[testT[k]]=VfO$validateur_probabilite[k]
                # 
                if(!Skip){
                  JsonObs=""
                  if(!is.na(testOO1[k])){
                    JsonObs=paste0(JsonObs,'"observateur_probabilite" : "',VfO$observateur_probabilite[k]
                                   ,'", "observateur_taxon" : { "$oid" : "',alldatataxa$`_id`[testOO1][k]
                                   ,'" },')  
                    
                  }
                  if(!is.na(testOO[k])){
                    
                    JsonObs=paste0(JsonObs,'"validateur_probabilite" : "',VfO$validateur_probabilite[k]
                                   ,'", "validateur_taxon" : { "$oid" : "',alldatataxa$`_id`[testOO][k]
                                   ,'" },')
                  }
                  JsonBefore=substr(ExpDataj,testObs+17,testF[testT[k]]-1)
                  JsonAfter=substr(ExpDataj,testF[testT[k]],testEndObs[length(testEndObs)])
                  JsonNewk=paste0(JsonBefore,JsonObs,JsonAfter)
                  JsonNewk=gsub("\\]\\]"," ] ",JsonNewk)
                  JsonNewk=gsub("\\[\\["," [ ",JsonNewk)
                  
                  
                  testU=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                       ,ListFichier[j],'"}')
                                          ,paste0('{"$set":{"observations":',JsonNewk,'}}'))
                  if(testU[[1]]!=1){stop("modification non prise en compte")}
                  
                  #stop("coder update time comme pour les sites/participations")
                  TimeUpdated=gsub(" ","T",Sys.time())
                  TimeUpdated=paste0(TimeUpdated,".000Z")
                  testU2=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                        ,ListFichier[j],'"}')
                                           ,paste0('{"$set":{"_updated" : { "$date" : "',TimeUpdated,'" }}}'))
                  if(testU2[[2]]!=1){stop("modification non prise en compte")}
                  
                  Obsjk<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',ListPart[i]
                                                      ,'"},"titre" : "'
                                                      ,ListFichier[j],'"}'))
                  Obsjk$observations[[1]]$tadarida_taxon_autre=NULL
                  test429=match(Obsjk$observations[[1]]$tadarida_taxon,alldatataxa$'_id')
                  Obsjk$observations[[1]]$tadarida_taxon=alldatataxa$libelle_court[test429]
                  if("validateur_taxon" %in% names(Obsjk$observations[[1]])){
                    test432=match(Obsjk$observations[[1]]$validateur_taxon,alldatataxa$'_id')
                    Obsjk$observations[[1]]$validateur_taxon=alldatataxa$libelle_court[test432]
                  }
                  if("observateur_taxon" %in% names(Obsjk$observations[[1]])){
                    test437=match(Obsjk$observations[[1]]$observateur_taxon,alldatataxa$'_id')
                    Obsjk$observations[[1]]$observateur_taxon=alldatataxa$libelle_court[test437]
                  }
                  print(ListFichier[j])
                  print(Obsjk$observations[[1]])
                  # if(length(testT)>1){
                  #   stop("check bonne integration")
                  # }
                  # # esel$valid.proba.new=VfO$validateur_probabilite[k]
                  # Easymatch=rbind(Easymatch,esel)
                }
              }else{
                #stop("code tadarida taxon absent")
                Obsjk<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',ListPart[i]
                                                    ,'"},"titre" : "'
                                                    ,ListFichier[j],'"}'))
                # Obsjk<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',"597b0b95ed506f000f712f5d"
                #                                     ,'"},"titre" : "'
                #                                     ,"Cir621406-2017-Pass1-Tron3-Chiro_0_00280_000",'"}'))
                # ExpObsjk=capture.output(donnee_obs$export(query=paste0('{"participation":{"$oid":"',"597b0b95ed506f000f712f5d"
                #                                                        ,'"},"titre" : "'
                #                                                        ,"Cir621406-2017-Pass1-Tron3-Chiro_0_00280_000",'"}'))
                # )
                # 
                
                Obsjko=Obsjk$observations[[1]]
                Obsjko$tadarida_taxon_autre=NULL
                
                if(!is.na(testOO1[k])){
                  print(ListPart[i])
                  print(ListFichier[j])
                  #stop("coder taxon observateur renseigne")
                  
                  if("observateur_taxon" %in% names(Obsjko)){
                    Obsjkd=subset(Obsjko,is.na(Obsjko$observateur_taxon))      
                  }else{
                    Obsjkd=Obsjko
                  }
                  if(k<length(testT)){
                    ResteAIntegrer0=alldatataxa$`_id`[testT0[(k+1):length(testT0)]]
                    Obsjkd=subset(Obsjkd,!(Obsjkd$tadarida_taxon %in% ResteAIntegrer0))
                    
                    #   ResteAIntegrer=alldatataxa$`_id`[testOO[(k+1):length(testT)]]
                    # Obsjkd=subset(Obsjkd,!(Obsjkd$tadarida_taxon %in% ResteAIntegrer))
                  }
                  if(nrow(Obsjkd)>0){
                    stop("coder taxon validateur disponible")
                    MatchTT=match(alldatataxa$`_id`[testOO1[k]],Obsjkd$tadarida_taxon)
                    #cas 402A : tadarida taxon disponible pour un match
                    if(!is.na(MatchTT)){
                      JsonObs=""
                      #if(!is.na(testOO1[k])){
                        JsonObs=paste0(JsonObs,'"observateur_probabilite" : "',VfO$observateur_probabilite[k]
                                       ,'", "observateur_taxon" : { "$oid" : "',alldatataxa$`_id`[testOO1][k]
                                       ,'" },')  
                        
                      #}
                      if(!is.na(testOO[k])){
                        
                        JsonObs=paste0(JsonObs,'"validateur_probabilite" : "',VfO$validateur_probabilite[k]
                                       ,'", "validateur_taxon" : { "$oid" : "',alldatataxa$`_id`[testOO][k]
                                       ,'" },')
                      }
                      JsonBefore=substr(ExpDataj,testObs+17,testF[MatchTT]-1)
                      JsonAfter=substr(ExpDataj,testF[MatchTT],testEndObs[length(testEndObs)])
                      JsonNewk=paste0(JsonBefore,JsonObs,JsonAfter)
                      JsonNewk=gsub("\\]\\]"," ] ",JsonNewk)
                      JsonNewk=gsub("\\[\\["," [ ",JsonNewk)
                      
                      
                      testU=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                           ,ListFichier[j],'"}')
                                              ,paste0('{"$set":{"observations":',JsonNewk,'}}'))
                      if(testU[[1]]!=1){stop("modification non prise en compte")}
                      
                      #stop("coder update time comme pour les sites/participations")
                      TimeUpdated=gsub(" ","T",Sys.time())
                      TimeUpdated=paste0(TimeUpdated,".000Z")
                      testU2=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                            ,ListFichier[j],'"}')
                                               ,paste0('{"$set":{"_updated" : { "$date" : "',TimeUpdated,'" }}}'))
                      if(testU2[[2]]!=1){stop("modification non prise en compte")}
                      
                      Obsjk<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',ListPart[i]
                                                          ,'"},"titre" : "'
                                                          ,ListFichier[j],'"}'))
                      Obsjk$observations[[1]]$tadarida_taxon_autre=NULL
                      test429=match(Obsjk$observations[[1]]$tadarida_taxon,alldatataxa$'_id')
                      Obsjk$observations[[1]]$tadarida_taxon=alldatataxa$libelle_court[test429]
                      if("validateur_taxon" %in% names(Obsjk$observations[[1]])){
                        test432=match(Obsjk$observations[[1]]$validateur_taxon,alldatataxa$'_id')
                        Obsjk$observations[[1]]$validateur_taxon=alldatataxa$libelle_court[test432]
                      }
                      if("observateur_taxon" %in% names(Obsjk$observations[[1]])){
                        test437=match(Obsjk$observations[[1]]$observateur_taxon,alldatataxa$'_id')
                        Obsjk$observations[[1]]$observateur_taxon=alldatataxa$libelle_court[test437]
                      }
                      print(ListFichier[j])
                      print(Obsjk$observations[[1]])
                      
                    }else{
                      #cas 402B : tadarida taxon non disponible pour un match
                      stop("coder tadarida taxon different 596")
                      
                    }
                    
                    
                    
                  }else{
                    #cas 402B : tadarida taxon non disponible, il faut créer une nouvelle ligne
                    #stop("code ligne obs non disponible")
                    if(grepl("5a9806e668062c000f9722ad",ExpDataj)){stop("coder evitement doublons empty")}
                    JsonObs=""
                    if(!is.na(testOO1[k])){
                      
                      JsonObs=paste0(JsonObs,'"observateur_probabilite" : "',VfO$observateur_probabilite[k]
                                     ,'", "observateur_taxon" : { "$oid" : "',alldatataxa$`_id`[testOO][k]
                                     ,'" }')
                    }
                    JsonBefore=substr(ExpDataj,testObs+17,testEndObs[length(testEndObs)]-1)
                    #JsonAfter=substr(ExpDataj,testEndObs[length(testEndObs)]-1,testEndObs[length(testEndObs)])
                    JsonNewk=paste0(JsonBefore,CreationLigneVide,JsonObs,"}]")
                    print(nchar(JsonNewk))
                    JsonNewk=gsub("\\]\\]"," ] ",JsonNewk)
                    print(nchar(JsonNewk))
                    JsonNewk=gsub("\\[\\["," [ ",JsonNewk)
                    print(nchar(JsonNewk))
                    
                    
                    testU=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                         ,ListFichier[j],'"}')
                                            ,paste0('{"$set":{"observations":',JsonNewk,'}}'))
                    if(testU[[1]]!=1){stop("modification non prise en compte 461")}
                    
                    #stop("coder update time comme pour les sites/participations")
                    TimeUpdated=gsub(" ","T",Sys.time())
                    TimeUpdated=paste0(TimeUpdated,".000Z")
                    testU2=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                          ,ListFichier[j],'"}')
                                             ,paste0('{"$set":{"_updated" : { "$date" : "',TimeUpdated,'" }}}'))
                    if(testU2[[2]]!=1){stop("modification non prise en compte 469")}
                    
                    Obsjk<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',ListPart[i]
                                                        ,'"},"titre" : "'
                                                        ,ListFichier[j],'"}'))
                    Obsjk$observations[[1]]$tadarida_taxon_autre=NULL
                    test429=match(Obsjk$observations[[1]]$tadarida_taxon,alldatataxa$'_id')
                    Obsjk$observations[[1]]$tadarida_taxon=alldatataxa$libelle_court[test429]
                    if("validateur_taxon" %in% names(Obsjk$observations[[1]])){
                      test432=match(Obsjk$observations[[1]]$validateur_taxon,alldatataxa$'_id')
                      Obsjk$observations[[1]]$validateur_taxon=alldatataxa$libelle_court[test432]
                    }
                    if("observateur_taxon" %in% names(Obsjk$observations[[1]])){
                      test437=match(Obsjk$observations[[1]]$observateur_taxon,alldatataxa$'_id')
                      Obsjk$observations[[1]]$observateur_taxon=alldatataxa$libelle_court[test437]
                    }
                    print(ListFichier[j])
                    print(Obsjk$observations[[1]])
                    
                    
                  }
                  
                  
                }else{
                  
                  if("validateur_taxon" %in% names(Obsjko)){
                    Obsjkd=subset(Obsjko,is.na(Obsjko$validateur_taxon))      
                  }else{
                    Obsjkd=Obsjko
                  }
                  if(k<length(testT)){
                    ResteAIntegrer0=alldatataxa$`_id`[testT0[(k+1):length(testT0)]]
                    Obsjkd=subset(Obsjkd,!(Obsjkd$tadarida_taxon %in% ResteAIntegrer0))
                    
                    #   ResteAIntegrer=alldatataxa$`_id`[testOO[(k+1):length(testT)]]
                    # Obsjkd=subset(Obsjkd,!(Obsjkd$tadarida_taxon %in% ResteAIntegrer))
                  }
                  if(nrow(Obsjkd)>0){
                    stop("coder taxon validateur disponible")
                    #cas 402A : tadarida taxon disponible pour un match
                    
                  }else{
                    #cas 402B : tadarida taxon non disponible, il faut créer une nouvelle ligne
                    #stop("code ligne obs non disponible")
                    if(grepl("5a9806e668062c000f9722ad",ExpDataj)){stop("coder evitement doublons empty")}
                    JsonObs=""
                    if(!is.na(testOO[k])){
                      
                      JsonObs=paste0(JsonObs,'"validateur_probabilite" : "',VfO$validateur_probabilite[k]
                                     ,'", "validateur_taxon" : { "$oid" : "',alldatataxa$`_id`[testOO][k]
                                     ,'" }')
                    }
                    JsonBefore=substr(ExpDataj,testObs+17,testEndObs[length(testEndObs)]-1)
                    #JsonAfter=substr(ExpDataj,testEndObs[length(testEndObs)]-1,testEndObs[length(testEndObs)])
                    JsonNewk=paste0(JsonBefore,CreationLigneVide,JsonObs,"}]")
                    print(nchar(JsonNewk))
                    JsonNewk=gsub("\\]\\]"," ] ",JsonNewk)
                    print(nchar(JsonNewk))
                    JsonNewk=gsub("\\[\\["," [ ",JsonNewk)
                    print(nchar(JsonNewk))
                    
                    
                    testU=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                         ,ListFichier[j],'"}')
                                            ,paste0('{"$set":{"observations":',JsonNewk,'}}'))
                    if(testU[[1]]!=1){stop("modification non prise en compte 461")}
                    
                    #stop("coder update time comme pour les sites/participations")
                    TimeUpdated=gsub(" ","T",Sys.time())
                    TimeUpdated=paste0(TimeUpdated,".000Z")
                    testU2=donnee_obs$update(query=paste0('{"participation":{"$oid":"',ListPart[i],'"},"titre" : "'
                                                          ,ListFichier[j],'"}')
                                             ,paste0('{"$set":{"_updated" : { "$date" : "',TimeUpdated,'" }}}'))
                    if(testU2[[2]]!=1){stop("modification non prise en compte 469")}
                    
                    Obsjk<-donnee_obs$find(query=paste0('{"participation":{"$oid":"',ListPart[i]
                                                        ,'"},"titre" : "'
                                                        ,ListFichier[j],'"}'))
                    Obsjk$observations[[1]]$tadarida_taxon_autre=NULL
                    test429=match(Obsjk$observations[[1]]$tadarida_taxon,alldatataxa$'_id')
                    Obsjk$observations[[1]]$tadarida_taxon=alldatataxa$libelle_court[test429]
                    if("validateur_taxon" %in% names(Obsjk$observations[[1]])){
                      test432=match(Obsjk$observations[[1]]$validateur_taxon,alldatataxa$'_id')
                      Obsjk$observations[[1]]$validateur_taxon=alldatataxa$libelle_court[test432]
                    }
                    if("observateur_taxon" %in% names(Obsjk$observations[[1]])){
                      test437=match(Obsjk$observations[[1]]$observateur_taxon,alldatataxa$'_id')
                      Obsjk$observations[[1]]$observateur_taxon=alldatataxa$libelle_court[test437]
                    }
                    print(ListFichier[j])
                    print(Obsjk$observations[[1]])
                    
                    
                  }
                }
                
              }
            }
          }
        }
      }
    }
  }  
  
  #déplacer le fichier traité
  NewName=paste0(DirDone,"/",basename(ValidFiles[w]))
  testC=file.rename(from=ValidFiles[w],to=NewName)
  if(!testC){stop("copy file to DoneDir")}
  
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
