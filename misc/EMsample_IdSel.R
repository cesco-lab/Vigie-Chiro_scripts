library(data.table)
ExpansionManuel=fread("DMsample.csv")
DataTot=fread("C:/wamp64/www/exportRP.csv")
VCCorr=fread("./mnhn/VCCorr.csv")
LSM=fread("ListSpManuel.csv")
tabase=fread("tabase3HF_France.csv")

ListSp=aggregate(ExpansionManuel$EFFECTIF,by=list(ExpansionManuel$ESPECE),sum)
ListSpY=aggregate(ExpansionManuel$EFFECTIF
                  ,by=c(list(ExpansionManuel$ESPECE),list(ExpansionManuel$ANNEE))
                  ,sum)

test=match(ExpansionManuel$ESPECE,LSM$ESPECE)
ExpansionManuel$Esp=LSM$Esp[test]

#ExpansionManuel=subset(ExpansionManuel,!is.na(ExpansionManuel$Esp))

VCPedestre=subset(VCCorr,(VCCorr$ID_CIRCUIT!=VCCorr$NewID)&
                    (VCCorr$NewID!="absent"))
test=match(ExpansionManuel$CIRCUIT,VCPedestre$ID_CIRCUIT)

ExpansionManuel$NewID=VCPedestre$NewID[test]
ExpansionManuel$NewITS=mapply(FUN=function(x,y,z) 
  gsub(paste0("Cir",x),paste0("Cir",y),z)
  ,ExpansionManuel$ID_CIRCUIT
  ,ExpansionManuel$NewID
  ,ExpansionManuel$ID_TRONCON_SESSION)


ExpansionManuel$ITS=ifelse(is.na(ExpansionManuel$NewID)
                           ,ExpansionManuel$ID_TRONCON_SESSION
                           ,ExpansionManuel$NewITS)

DataTot=subset(DataTot,substr(DataTot$donnee,1,3)=="Cir")

#fwrite(DataTot,"C:/wamp64/www/exportRP.csv",sep=";")

test=grepl("temps_debut",colnames(DataTot))
if(sum(test)==2)
{
  test2=match("temps_debut",colnames(DataTot))
  colnames(DataTot)[test2+1]="temps_fin"
}

EM_match=vector()
EMreorder=ExpansionManuel[0,]
EMmissing=ExpansionManuel[0,]
Pchannelproblem=vector()
IdSel=data.frame()
for (h in 1:nlevels(as.factor(ExpansionManuel$ITS)))
{
  EMh=subset(ExpansionManuel
             ,ExpansionManuel$ITS==levels(as.factor(ExpansionManuel$ITS))[h])
  Sys.time()
  IAh=subset(DataTot,grepl(levels(as.factor(ExpansionManuel$ITS))[h]
                           ,DataTot$donnee))
  Sys.time()
  
  if(nrow(IAh)>0)
  {
    print(paste(h,nlevels(as.factor(ExpansionManuel$ITS)),Sys.time()))
    
    table(IAh$espece)
    IAh$Mic=tstrsplit(IAh$donnee,"_")[[2]]
    table(IAh$Mic)
    
    MaxDur=aggregate(IAh$temps_fin,by=list(IAh$Mic),FUN=max)
    CanalExp=subset(MaxDur$Group.1,MaxDur$x<0.6)
    if(length(CanalExp)>0)
    {
      IAh=subset(IAh,IAh$Mic %in% CanalExp[1])
      IAh$TimeChar=tstrsplit(IAh$donnee,"_")[[3]]
      IAh$TimeNum=as.numeric(IAh$TimeChar)
      IAh$TimeSpS=IAh$TimeNum+IAh$temps_debut*10
      IAh$TimeSpE=IAh$TimeNum+IAh$temps_fin*10
      
      Sys.time()
      if(nrow(IAh)>0)
      {
        for (i in 1:nrow(EMh))
        {
          EMi=EMh[i,]
          EMtimeS=EMi$TEMPS
          if(is.na(EMi$NewID)){Window=4}else{Window=18} #for difference in acquisition time in the two protocols
          EMtimeE=EMtimeS+Window
          IAmatch=subset(IAh,(IAh$TimeSpE>EMtimeS)&(IAh$TimeSpS<EMtimeE))
          if(nrow(IAmatch)>0)
          {
            IAsp=subset(IAmatch,IAmatch$espece==EMi$Esp)
            if(nrow(IAsp)>0)
            {
              IASel=IAsp[sample.int(nrow(IAsp),1),]
            }else{
              IASel=IAmatch[sample.int(nrow(IAmatch),1),]
            }
            
            IdSel=rbind(IdSel,IASel)
          }else{
            EMmissing=rbind(EMmissing,EMi)
          }
        }
      }else{
        EMmissing=rbind(EMmissing,EMh)
      }
    }else{
      EMmissing=rbind(EMmissing,EMh)
    }
  }else{
    EMmissing=rbind(EMmissing,EMh)
  }
  
}

#purge de la RSDB
donneeRSDB=gsub(".wav","",tabase$Filename)
donneeRSDB=levels(as.factor(donneeRSDB))
test=subset(donneeRSDB,grepl("Cir",donneeRSDB))
IdSelpurge=subset(IdSel,!(IdSel$donnee %in% donneeRSDB))

fwrite(IdSelpurge,"IdSelE.csv",sep=";")

table(EMmissing$ID_CIRCUIT)


