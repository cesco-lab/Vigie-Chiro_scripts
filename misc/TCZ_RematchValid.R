library(data.table)
library(archive)
#ListTCZ=list.files("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/tcz",full.names=T)
ListTCZ=list.files("C:/wamp64/www/tcz",full.names=T)
#Part123=fread("ListParValid.csv")
DataE=fread("DataE.csv")
ETOT=fread("./Tadarida/rounds/compli_validation190220/EXHAUSTIFS_TOTAL_ded.csv")
Exhaustif=T
Donnees_E=levels(as.factor(ETOT$`nom du fichier`))

##ATTENTION NE TRAITE PAS POUR L'INSTANT les validations en surnombre par rapport aux prédictions (= non matchables)

ProbaValid=data.frame()

for (i in 1:length(Donnees_E))
{
  Datatemp=subset(DataE,DataE$donnee %in% Donnees_E[i])
  Partemp=unique(Datatemp$participation)
  if(length(Partemp)>1)
  {
    print(Partemp)
    #stop("DOUBLONS !!!")
  }else{
    if(length(Partemp)==1){
      TCZmatch=subset(ListTCZ,grepl(Partemp,ListTCZ))
      if(length(TCZmatch)==1){
        
        FI=file.info(TCZmatch)
        
        if(FI$size>100)
        {
         ProbaValid=data.frame()
          print(paste(i,Sys.time()))
          Validtemp=subset(ETOT,ETOT$`nom du fichier` %in% Donnees_E[i])
          TCName=paste0(Partemp,"/",Donnees_E[i],".tc")
          x <- archive::archive_read(archive = TCZmatch,file=TCName)
          #x=untar(TCZmatch,file=TCName) 
          Scoretemp=readr::read_csv(x)
          ScoreSel=as.matrix(subset(Scoretemp,select=Validtemp$SpReMatch))
          ScoreDispo=ScoreSel
          ProbaDispo=Scoretemp
          #if((nrow(Validtemp)<(nrow(Scoretemp)))&(nrow(Validtemp)>1)){stop(i)}
          RowSelected=0
          Sp_toRematch=Validtemp$SpReMatch
          for (j in 1:min(nrow(ScoreSel),nrow(Validtemp)))
          {
            if(RowSelected!=0)
            {
              ScoreDispo=ScoreDispo[-RowSelected,]
              if(is.matrix(ScoreDispo))              
              {
                ScoreDispo=ScoreDispo[,-ColSelected]
              }else{
                ScoreDispo=ScoreDispo[-ColSelected]
              }
              ProbaDispo=ProbaDispo[-RowSelected,]
              Sp_toRematch=subset(Sp_toRematch
                                  ,Sp_toRematch!=ValidSel$SpReMatch)
              #Sp_toRematch=Sp_toRematch[-ValidSel$SpReMatch]
              
            }
            PosSel=which(ScoreDispo == max(ScoreDispo), arr.ind = TRUE)
            if(length(PosSel)>1)
            {
              ProbaSel=ProbaDispo[PosSel[1,1],]
              ValidSel=subset(Validtemp,Validtemp$SpReMatch==colnames(ScoreDispo)[PosSel[1,2]])
              RowSelected=PosSel[1,1]
              ColSelected=PosSel[1,2]
            }else{
              if(length(Sp_toRematch)==1){
                ProbaSel=ProbaDispo[PosSel,]
                #if(PosSel>1){stop("test ValidSel")}
                
                ValidSel=subset(Validtemp,Validtemp$SpReMatch==Sp_toRematch)
                RowSelected=PosSel
                ColSelected=1
              }else{
                ProbaSel=ProbaDispo
                #stop("test ValidSel")
                ValidSel=subset(Validtemp,Validtemp$SpReMatch==names(ScoreDispo)[PosSel[1]])
                RowSelected=1
                ColSelected=PosSel
                
              }
              
            }
            
            ProbaSel$ValidId=ValidSel$SpReMatch
            ProbaSel$ValidConf=ValidSel$validateur_probabilite
            ProbaValid=rbind(ProbaValid,ProbaSel)
          }
          if(nrow(ProbaDispo)>1)
          {
            ProbaDispo=ProbaDispo[-RowSelected,]
            ScoreDispo=subset(ScoreSel,Scoretemp$SpMaxF2 %in% ProbaDispo$SpMaxF2)
            Nsupp=nrow(ProbaDispo)
            if(Nsupp>1){stop("test Nsupp")}
            for (j in 1:Nsupp)
            {
              PosSel=which(ScoreDispo == max(ScoreDispo), arr.ind = TRUE)
              if(length(PosSel)>1)
              {
                ProbaSel=ProbaDispo[PosSel[1,1],]
                ValidSel=subset(Validtemp,Validtemp$SpReMatch==colnames(ScoreDispo)[PosSel[1,2]])
                RowSelected=PosSel[1,1]
              }else{
                ProbaSel=ProbaDispo[PosSel]
                stop("test ScoreDispo")
                ValidSel=subset(Validtemp,Validtemp$SpReMatch==colnames(ScoreDispo)[PosSel[1]])
                RowSelected=PosSel
              }
            }
            ProbaSel$ValidId=ValidSel$SpReMatch
            ProbaSel$ValidConf=ValidSel$validateur_probabilite
            ProbaValid=rbind(ProbaValid,ProbaSel)
          }
          
        }
        
      }
      
      
      
    }else{
      if(length(TCZmatch)>1){
        print(TCZmatch)
        stop("DOUBLONS !!!")
      }
    }
  }
}
