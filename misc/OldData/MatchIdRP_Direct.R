library(data.table)
#ETV1=fread("./Tadarida/rounds/compli_validation190220/ETVtot.csv")
#ETV2=fread("./Tadarida/rounds/compli_validation190220/EXHAUSTIFS_TOTAL_ded.csv")
DataRP=fread("DataLP_RP.csv")
IdExp=fread("./mnhn/CHIRO_DIRECT_2014.csv")
IdExp=fread("IdD2020-07-28.csv")
TempsExp=T
VCcorr=fread("./mnhn/VCcorr.csv")
SpeciesList=fread("SpeciesList.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
ConfusionPairs=fread("ConfsionSpeciesPairs.csv")
IdExp$ESPECE[IdExp$ESPECE=="Hypsugo_savi"]="Hypsugo savii"
IdExp$ESPECE[IdExp$ESPECE=="Miniopterus_schreibersi"]="Miniopterus_schreibersii"
IdExp$ESPECE[IdExp$ESPECE=="Myotis_brantii"]="Myotis_brandtii"
IdExp$ESPECE[IdExp$ESPECE=="Myotis_daubentoni"]="Myotis_daubentonii"
IdExp$ESPECE[IdExp$ESPECE=="Myotis_puniclus"]="Myotis_punicus"
IdExp$ESPECE[IdExp$ESPECE=="nyctalus_noctula"]="Nyctalus_noctula"
IdExp$ESPECE[IdExp$ESPECE=="Pipistrellus_kulhii"]="Pipistrellus_kuhlii"
IdExp$ESPECE[IdExp$ESPECE=="Pipistrellus_nathusius"]="Pipistrellus_nathusii"
IdExp$ESPECE[IdExp$ESPECE=="pipistrellus_pipistrellus"]="Pipistrellus_pipistrellus"
IdExp$ESPECE[IdExp$ESPECE=="Plecotus_auricus"]="Plecotus_auritus"
IdExp$ESPECE[IdExp$ESPECE=="Plecotus_macrobularis"]="Plecotus_macrobullaris"
IdExp$ESPECE[IdExp$ESPECE=="Vesprtilio_murinus"]="Vespertilio_murinus"
IdExp$ESPECE[IdExp$ESPECE=="Eptesicus_nilsonii"]="Eptesicus_nilssonii"
IdExp$ESPECE[IdExp$ESPECE=="nyctalus_leisleri"]="Nyctalus_leisleri"
IdExp$ESPECE[IdExp$ESPECE=="Pipistrellus_Pipistrellus"]="Pipistrellus_pipistrellus"
IdExp$ESPECE[IdExp$ESPECE=="Myotis_natterii"]="Myotis_nattereri"
IdExp$ESPECE[IdExp$ESPECE=="Pipistrellus_Pipistrellus"]="Pipistrellus_pipistrellus"
IdExp$ESPECE[IdExp$ESPECE=="Pipistrellus_Pipistrellus"]="Pipistrellus_pipistrellus"
IdExp$ESPECE[IdExp$ESPECE=="Pipistrellus kulhii"]="Pipistrellus kuhlii"


IdExp$SciName=gsub("_"," ",IdExp$ESPECE)
test=match(IdExp$SciName,SpeciesList$`Scientific name`)
table(subset(IdExp$ESPECE,is.na(test)))

DataChecked=data.frame()
for (i in 1:length(unique(IdExp$ID_CIRCUIT)))
{
  print(paste(unique(IdExp$ID_CIRCUIT)[i],Sys.time()))
  Idi=subset(IdExp,IdExp$ID_CIRCUIT==unique(IdExp$ID_CIRCUIT)[i])
  NumNew=VCcorr$NewID[VCcorr$ID_CIRCUIT==unique(IdExp$ID_CIRCUIT)[i]]
  if(length(NumNew)==0){
    NumNew=unique(IdExp$ID_CIRCUIT)[i]
  }else{
    if(!is.na(NumNew[1]))
    {
      if(NumNew=="absent"){NumNew=unique(IdExp$ID_CIRCUIT)[i]}
    }
  }
  ProtPedestre=as.numeric(NumNew)>1000
  NumNew=ifelse(as.numeric(NumNew)>1000,paste0("Vigiechiro - PÃÂ©destre-"
                                               ,NumNew)
                ,paste0("Vigie-chiro - Routier-",NumNew))
  Datai=subset(DataRP,DataRP$site==NumNew)
  for (j in 1:length(unique(Idi$ANNEE)))
  {
    Idj=subset(Idi,Idi$ANNEE==unique(Idi$ANNEE)[j])
    Dataj=subset(Datai,substr(Datai$date_debut,7,10)==unique(Idi$ANNEE)[j])
    if(nrow(Dataj)>0)
    {
      Pass=tstrsplit(Dataj$donnee,split="-")[[3]]
      Pass=gsub("Pass","",Pass)
      for (k in 1:length(unique(Idi$PASSAGE)))
      {
        Idk=subset(Idj,Idj$PASSAGE==unique(Idj$PASSAGE)[k])
        Datak=subset(Dataj,Pass==unique(Idj$PASSAGE)[k])
        if(nrow(Datak)>0)
        {
          Partk=unique(Datak$participation)
          if(length(Partk)==1)
          {
            Canal=Particip$canal_enregistrement_direct[Particip$participation==Partk]
            if(Canal=="GAUCHE"){
              
              Datak=subset(Datak,!Datak$Datamicro)
            }else{
              if(Canal=="DROITE"){
                Datak=subset(Datak,Datak$Datamicro)
              }else{
                Datak=Datak[0,]
              }
            }
            if(nrow(Datak)>0)
            {
              if("NÂ° du tronÃ§on"  %in% names(Idk))
              {
                Tronk=Idk$`NÂ° du tronÃ§on`
              }else{
                Tronk=tstrsplit(Idk$ID_TRONCON_SESSION,split="Tron")[[2]]
              }
              DTronk=tstrsplit(Datak$donnee,split="-")[[4]]
              DTronk=gsub("Tron","",DTronk)
              
              
              for (l in 1:length(unique(Tronk)))
              {
                Idl=subset(Idk,Tronk==unique(Tronk)[l])
                Datal=subset(Datak,DTronk==unique(Tronk)[l])
                if(nrow(Datal)>0)
                {
                  
                  LineToSuppress=vector()
                  for (m in 1:nrow(Idl))
                  {
                    SpId=SpeciesList$Nesp2[SpeciesList$`Scientific name`==
                                             Idl$SciName[m]][1]
                    Datal$TEMPS=as.numeric(substr(Datal$donnee,nchar(Datal$donnee)-6
                                                  ,nchar(Datal$donnee)-4))
                    Datal=Datal[order(Datal$TEMPS),]
                    if(TempsExp)
                    {
                      DataInt=subset(Datal
                                     ,(Datal$TEMPS>(as.numeric(Idl$TEMPS_DEBUT[m]/10)-5))&
                                       (Datal$TEMPS<(as.numeric(Idl$TEMPS_FIN[m]/10)+1)))
                    }else{
                      DataInt=subset(Datal
                                     ,(Datal$TEMPS>(as.numeric(Idl$TEMPS_DEBUT[m])-5))&
                                       (Datal$TEMPS<(as.numeric(Idl$TEMPS_FIN[m])+1)))
                      
                    }
                    
                    DataSp=subset(DataInt,DataInt$espece==SpId)
                    if(nrow(DataSp)>0)
                    {
                      DataSp$obs.espece[1]=SpId
                      DataSp$obs.proba[1]=ifelse(Idl$DEGRES_CONFIANCE[m]==0
                                                 ,"POSSIBLE","SUR")
                      Datal=subset(Datal,(Datal$donnee!=DataSp$donnee[1])|
                                     (Datal$espece!=DataSp$espece[1]))
                      DataChecked=rbind(DataChecked,DataSp[1,])
                      LineToSuppress=c(LineToSuppress,m)
                    }
                  }
                  Idl_woMatch=Idl[-LineToSuppress,]
                  if(nrow(Idl_woMatch)>0)
                  {
                    for (m in 1:nrow(Idl))
                    {
                      SpId=SpeciesList$Nesp2[SpeciesList$`Scientific name`==
                                               Idl$SciName[m]]
                      if(length(SpId)==1){
                        if(TempsExp)
                        {
                          DataInt=subset(Datal
                                         ,(Datal$TEMPS>(as.numeric(Idl$TEMPS_DEBUT[m]/10)-5))&
                                           (Datal$TEMPS<(as.numeric(Idl$TEMPS_FIN[m]/10)+1)))
                        }else{
                          DataInt=subset(Datal
                                         ,(Datal$TEMPS>(as.numeric(Idl$TEMPS_DEBUT[m])-5))&
                                           (Datal$TEMPS<(as.numeric(Idl$TEMPS_FIN[m])+1)))
                          
                        }
                        ConfusionOrder=subset(ConfusionPairs
                                              ,ConfusionPairs$Species==SpId)
                        ReOrder=match(DataInt$espece,ConfusionOrder$SpeciesId)
                        DataInt=DataInt[order(ReOrder),]
                        
                        DataInt$obs.espece[1]=SpId
                        DataInt$obs.proba[1]=ifelse(Idl$DEGRES_CONFIANCE[m]==0
                                                    ,"POSSIBLE","SUR")
                        Datal=subset(Datal,(Datal$donnee!=DataInt$donnee[1])|
                                       (Datal$espece!=DataInt$espece[1]))
                        DataChecked=rbind(DataChecked,DataInt[1,])
                      }
                    }
                  }
                }
              }
              
              
            }
            
          }
        }
      }
    }
  }
}

table(tstrsplit(DataChecked$donnee,split="-")[[1]])
table(DataChecked$espece)

fwrite(DataChecked,"DataCheckedDirect2.csv",sep=";")
