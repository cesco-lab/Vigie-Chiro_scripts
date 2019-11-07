library(data.table)

ExpansionManuel=fread("./mnhn/CHIRO_DIRECT_2014.csv")
table(ExpansionManuel$TYPE_SUIVI)
IdAuto=fread("TableTCtot_BonneVersion.csv")
VCCorr=fread("./mnhn/VCCorr.csv")
LSM=fread("ListSpManuel.csv")

ListSp=aggregate(ExpansionManuel$REF_CHIRO,by=list(ExpansionManuel$ESPECE)
                 ,length)
ListSpY=aggregate(ExpansionManuel$REF_CHIRO
                  ,by=c(list(ExpansionManuel$ESPECE),list(ExpansionManuel$ANNEE))
                  ,length)

test=match(ExpansionManuel$ESPECE,LSM$ESPECE)
ExpansionManuel$Esp=LSM$Esp[test]

ExpansionManuel=subset(ExpansionManuel,!is.na(ExpansionManuel$Esp))

VCPedestre=subset(VCCorr,(VCCorr$ID_CIRCUIT!=VCCorr$NewID)&
                    (VCCorr$NewID!="absent"))
test=match(ExpansionManuel$ID_CIRCUIT,VCPedestre$ID_CIRCUIT)
summary(test)

ExpansionManuel$NewID=VCPedestre$NewID[test]
ExpansionManuel$NewITS=mapply(FUN=function(x,y,z) 
  gsub(paste0("Cir",x),paste0("Cir",y),z)
  ,ExpansionManuel$ID_CIRCUIT
  ,ExpansionManuel$NewID
  ,ExpansionManuel$ID_TRONCON_SESSION)


ExpansionManuel$ITS=ifelse(is.na(ExpansionManuel$NewID)
                           ,ExpansionManuel$ID_TRONCON_SESSION
                           ,ExpansionManuel$NewITS)

table(ExpansionManuel$TYPE_SUIVI)

EM_match=vector()
EMreorder=ExpansionManuel[0,]
EMmissing=ExpansionManuel[0,]
Pchannelproblem=vector()
IdMatch=data.frame()
for (h in 1:nlevels(as.factor(ExpansionManuel$ITS)))
{
  EMh=subset(ExpansionManuel
             ,ExpansionManuel$ITS==levels(as.factor(ExpansionManuel$ITS))[h])
  Sys.time()
  IAh=subset(IdAuto,grepl(levels(as.factor(ExpansionManuel$ITS))[h]
                          ,IdAuto$Group.1))
  Sys.time()
  
  if(nrow(IAh)>0)
  {
    print(paste(h,nlevels(as.factor(ExpansionManuel$ITS)),Sys.time()))
    
    table(IAh$SpMax2)
    IAh$Mic=tstrsplit(IAh$Group.1,"_")[[2]]
    table(IAh$Mic)
    
    MaxDur=aggregate(IAh$Tend,by=list(IAh$Mic),FUN=max)
    CanaDirect=subset(MaxDur$Group.1,MaxDur$x>0.5)
    if(length(CanaDirect)>0)
    {
      IAh=subset(IAh,IAh$Mic %in% CanaDirect[1])
      IAh$TimeChar=tstrsplit(IAh$Group.1,"_")[[3]]
      IAh$TimeNum=as.numeric(IAh$TimeChar)
      IAh$TimeSpS=IAh$TimeNum+IAh$Tstart
      IAh$TimeSpE=IAh$TimeNum+IAh$Tend
      
      Sys.time()
      if(nrow(IAh)>0)
      {
        for (i in 1:nrow(EMh))
        {
          EMi=EMh[i,]
          IAmatch=subset(IAh,(IAh$TimeSpE>EMi$TEMPS_DEBUT))
          IAmatch=subset(IAmatch,IAmatch$TimeSpS<as.numeric(EMi$TEMPS_FIN))
          IAmatch$SpMax2
          if(sum(grepl(EMi$Esp,colnames(IAmatch)))>0)
          {
          ScoreSp=subset(IAmatch,select=EMi$Esp)
          IAsel=IAmatch[which.max(as.data.frame(ScoreSp)[,1]),]
          IAsel$validateur_taxon=EMi$Esp
          IAsel$validateur_probabilite=EMi$DEGRES_CONFIANCE
          IdMatch=rbind(IdMatch,IAsel)
          }
        }
      }else{
        EMmissing=rbind(EMmissing,EMh)
      }
    }else{
      Pchannelproblem=c(Pchannelproblem,IAh$participation[1])
    }
    
  }
}

fwrite(IdMatch,"IdMatchD.csv")
