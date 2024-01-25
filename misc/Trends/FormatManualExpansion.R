library(data.table)
library(readxl)

#ExpManual=fread("C:/Users/yvesb/Documents/mnhn/ExpansionManuel.csv")
ExpManual=read_xlsx("C:/Users/yvesb/Documents/mnhn/SECTEUR_Expansion_2015_V1.xlsx")
CorrVC=fread("C:/Users/yvesb/Documents/mnhn/VC_Corr.csv")
#Particip=fread("p_export_forLinux.csv")
DataT=fread("DataTrends.csv")

DataTRP=subset(DataT,DataT$protocole!="POINT_FIXE")
DataTRPe=subset(DataTRP,DataTRP$expansion_direct=="expansion")

test=max("5889e8c17ac9bdd021000039" %in% DataT$participation)

#table(ExpManual$espece)

table((ExpManual$VIDE))
table(is.na(ExpManual$VIDE))
#ExpManual$VIDE[is.na(ExpManual$VIDE)]=2
#table(ExpManual$VIDE,ExpManual$Activite)
barplot(table(ExpManual$VIDE,ExpManual$ANNEE))
test=as.data.table(table(ExpManual$VIDE,ExpManual$ANNEE))
test2=dcast(test,V2~V1)
plot(test2$'TRUE'/(test2$'FALSE'+test2$'TRUE'))

##Ventiler les espèces

Sampling=as.data.frame(ExpManual[,1:9])

ExpManualTable=data.frame()
for (i in 10:28){
  Samplingi=Sampling
  Samplingi=cbind(Samplingi,ExpManual[,i])
  names(Samplingi)[ncol(Samplingi)]="nb_contacts"
  Samplingi$espece=names(ExpManual)[i]
  ExpManualTable=rbind(ExpManualTable,Samplingi)
}
summary(ExpManualTable$nb_contacts)
summary(is.na(ExpManualTable$nb_contacts))
ExpManualTable$espece[ExpManualTable$espece=="Myospp"]="Myotis_sp"
ExpManualTable$espece[ExpManualTable$espece=="Pipistrellus_kulhii"]="Pipistrellus_kuhlii"
ExpManualTable$espece[ExpManualTable$espece=="Pipistrellus_nathusius_kulhii"]="Pipistrellus_kuhlii"
ExpManualTable$espece[ExpManualTable$espece=="Plecotus_sp"]="Plecotus_austriacus"


NameSp=tstrsplit(ExpManualTable$espece,split="_")
CodeSp=paste0(substr(NameSp[[1]],1,3),substr(NameSp[[2]],1,3))
table(CodeSp)
ExpManualTable$espece=CodeSp

ExpT=aggregate(ExpManualTable$nb_contacts,by=c(list(ExpManualTable$ID_CIRCUIT)
                                               ,list(ExpManualTable$NUMEROS_TRONCON)
                                               ,list(ExpManualTable$ANNEE)
                                               ,list(ExpManualTable$NUMEROS_PASSAGE)
                                               ,list(ExpManualTable$espece)
                                               ,list(ExpManualTable$TYPE_SUIVI)),sum)
names(ExpT)=c("site","Tron","year","passage","espece","protocole","nb_contacts")
head(ExpT)
test=match(ExpT$site,CorrVC$ID_CIRCUIT)
summary(test)
SiteManquant=subset(ExpT,is.na(test))
table(SiteManquant$site,SiteManquant$protocole)
#changer les noms de sites
ExpT$site=ifelse(is.na(test),ExpT$site,CorrVC$NewID[test])
#table(ExpT$site)
ExpT$site=ifelse(ExpT$protocole=="routier",paste0("Vigie-chiro - Routier-",ExpT$site)
                 ,paste0("Vigiechiro - Pédestre-",ExpT$site))

#retrouver les dates
SamplingDate=subset(ExpT,select=c("site","year","passage"))
SamplingDate=unique(SamplingDate)
# SamplingDate$participation=""
# SamplingDate$num_micro=0
# SamplingDate$mat=""
# SamplingDate$temps_enr=360
# SamplingDate$julian=220
# SamplingDate$score_max=0
# SamplingDate$id_site=""
# SamplingDate$date="01/01/2006 00:00"
# SamplingDate$idobservateur=""
# SamplingDate$nb_wav=0
# SamplingDate$nb_ta=0
# SamplingDate$nb_tc=0
# SamplingDate$nb_don=0
# SamplingDate$sps=0
# SamplingDate$dif_wav_ta=0
DataCompl=DataTRP[0,]
for (j in 1:nrow(SamplingDate)){
  Dataj=subset(DataTRP,DataTRP$year==SamplingDate$year[j])
  Dataj=subset(Dataj,Dataj$site==SamplingDate$site[j])
  table(Dataj$julian,Dataj$date_debut)
  if(length(unique(Dataj$julian))>1){
    DateOrdered=unique(Dataj$julian)[order(unique(Dataj$julian))]
    
    if(length(unique(Dataj$julian))>=SamplingDate$passage[j]){
      Dataj=subset(Dataj,Dataj$julian==DateOrdered[SamplingDate$passage[j]])
      
    }else{
      #stop("pas assez de date")
      Dataj=subset(Dataj,Dataj$julian==DateOrdered[length(DateOrdered)])
      
    }
    
    
  }  
  
  if(nrow(Dataj)==0){
    #stop("pas de data")
    Dataj=DataTRP[1,]
    Dataj$participation="manquante"
    Dataj$site=SamplingDate$site[j]
    Dataj$year=SamplingDate$year[j]
    
      }
  
  DataSel=Dataj[1,]
  DataCompl=rbind(DataCompl,DataSel)
  if(j%%50==1){
    print(j)
    print(Sys.time())
    #print(nrow(DataCompl))
    
  }
  
}

test=max("5889e8c17ac9bdd021000039" %in% DataCompl$participation)


DataCompl$year=NULL
DataCompl$site=NULL
DataCompl$protocole=NULL
DataCompl$Tron=NULL
DataCompl$espece=NULL
DataCompl$protocole=NULL
DataCompl$nb_contacts=NULL
DataCompl2=cbind(DataCompl,SamplingDate)

ExpComplete=merge(ExpT,DataCompl2,by=c("site","year","passage"))

ExpComplete$num_micro=ifelse(ExpComplete$participation=="manquante",0,ExpComplete$num_micro)
ExpComplete$mat=ifelse(ExpComplete$participation=="manquante","Tran",ExpComplete$mat)
ExpComplete$mat[ExpComplete$mat==""]="Tran"

ExpComplete$temps_enr=ifelse(ExpComplete$participation=="manquante"
                            ,ifelse(ExpComplete$protocole=="routier",300,360)
                            ,ExpComplete$temps_enr)

ExpComplete$protocole[ExpComplete$protocole=="PedestreRoutier"]="pedestre"

ExpComplete=subset(ExpComplete,ExpComplete$temps_enr>180)
hist(ExpComplete$temps_enr)
table(ExpComplete$protocole)
ExpComplete$julian=ifelse(ExpComplete$participation=="manquante"
                          ,ifelse(ExpComplete$passage==1,190,250),ExpComplete$julian)
hist(ExpComplete$julian)
summary(is.na(ExpComplete$julian))
table(ExpComplete$julian)
ExpComplete$cjulian=ifelse(ExpComplete$participation=="manquante"
                          ,ifelse(ExpComplete$passage==1,-0.9916763,-0.3975336),ExpComplete$cjulian)

ExpComplete$cjulian2=ifelse(ExpComplete$participation=="manquante"
                           ,ifelse(ExpComplete$passage==1,0.983422,0.1580329)
                           ,ExpComplete$cjulian2)

ExpComplete$sjulian=ifelse(ExpComplete$participation=="manquante"
                           ,ifelse(ExpComplete$passage==1,-0.1287558,-0.9175876),ExpComplete$sjulian)

ExpComplete$sjulian2=ifelse(ExpComplete$participation=="manquante"
                            ,ifelse(ExpComplete$passage==1, 0.01657805,0.8419671)
                            ,ExpComplete$sjulian2)


ExpComplete$idsite=ifelse(ExpComplete$participation=="manquante","",ExpComplete$idsite)
ExpComplete$score_max=ifelse(ExpComplete$participation=="manquante",0,ExpComplete$score_max)
ExpComplete$idobservateur=ifelse(ExpComplete$participation=="manquante","Tran"
                                 ,ExpComplete$idobservateur)
ExpComplete$id_observateur=ifelse(ExpComplete$participation=="manquante","Tran"
                                 ,ExpComplete$id_observateur)

ExpComplete$temperature_debut=ifelse(ExpComplete$participation=="manquante",NA
                                     ,ExpComplete$temperature_debut)
ExpComplete$temperature_fin=ifelse(ExpComplete$participation=="manquante",NA
                                     ,ExpComplete$temperature_fin)
ExpComplete$vent=ifelse(ExpComplete$participation=="manquante",NA
                                     ,ExpComplete$vent)
ExpComplete$couverture=ifelse(ExpComplete$participation=="manquante",NA
                                     ,ExpComplete$couverture)
ExpComplete$detecteur_enregistreur_type=ifelse(ExpComplete$participation=="manquante"
                                               ,""
                                               ,ExpComplete$detecteur_enregistreur_type)

ExpComplete$'num site'=ifelse(ExpComplete$participation=="manquante"
                                               ,""
                                               ,ExpComplete$'num site')

ExpComplete$email=ifelse(ExpComplete$participation=="manquante"
                            ,""
                            ,ExpComplete$email)

ExpComplete$id_protocole=ifelse(ExpComplete$participation=="manquante"
                         ,""
                         ,ExpComplete$id_protocole)

ExpComplete$longitude=ifelse(ExpComplete$participation=="manquante"
                                ,0
                                ,ExpComplete$longitude)

ExpComplete$latitude=ifelse(ExpComplete$participation=="manquante"
                             ,0
                             ,ExpComplete$latitude)

ExpComplete$yearF2=ifelse(ExpComplete$participation=="manquante"
                            ,0
                            ,ExpComplete$yearF2)

ExpComplete$yearF4=ifelse(ExpComplete$participation=="manquante"
                          ,0
                          ,ExpComplete$yearF4)

ExpComplete$siteloc=paste(ExpComplete$site,ExpComplete$Tron)

ExpComplete$passage=NULL

test=ExpComplete$nb_contacts/ExpComplete$temps_enr*5

ExpComplete$nb_contacts=pmin(ExpComplete$nb_contacts,floor(ExpComplete$temps_enr/5))

fwrite(ExpComplete,"ExpansionManualForTrends.csv",sep=";")

