library(data.table)
SeuilFiable=0 #? ?diter 
Sectorized=T
#SeuilFiable=""
#ETAPE 0 - IMPORT DES TABLES
#bien renommer les chemins en fonction de l'ordi utilis?
#table "donn?es"
Sys.time()
DataLP=fread("E:/RP/DataLP_RP_Sectorized_Valid.csv")
Sys.time()
#table "seuils"
#RefSeuils=fread("Referentiel_seuils_ProbEspHF_1510_Cir.csv")

#table "esp?ces"
#GroupList_prev=fread("GroupList_HF.csv") 
GroupList=fread("C:/Users/yvesb/Tadarida/Tadarida-C/tadaridaC_src/other_inputs/SpeciesList.csv") 

LatMin=0
LatMax=90
LongMin=-180
LongMax=180


#pour afficher les milisecondes
op <- options(digits.secs=3)
#pour reset
#options(op)


#ETAPE 0b - tri des participations foireuses (dur?e s?quence Pip + proba Pip : A AFFINER!!!!)
#A FAIRE : tri sur le sampling rate
Sys.time()
DataPip=subset(DataLP,substr(DataLP$espece,1,3)=="Pip") #3 sec
Sys.time()
DurSeq=DataPip$temps_fin-DataPip$temps_debut
Q90PipDur=aggregate(DurSeq,by=list(DataPip$participation,DataPip$Datamicro)
                    ,FUN=function(x) quantile(x,0.5))
MaxPipProb=aggregate(DataPip$probabilite,by=list(DataPip$participation,DataPip$Datamicro)
                     ,FUN=max)
hist(MaxPipProb$x,breaks=50)
Q90PipProb=aggregate(DataPip$probabilite,by=list(DataPip$participation,DataPip$Datamicro)
                     ,FUN=function(x) quantile(x,0.9))
SelQ90Pip=subset(Q90PipDur,(Q90PipDur$x>0.2)&(Q90PipProb$x>0.33))
Sys.time()
test=match(paste(DataLP$participation,DataLP$Datamicro)
           ,paste(SelQ90Pip$Group.1,SelQ90Pip$Group.2)) # 6 sec
test2=(is.na(test))
Sys.time()
DataLP$Fiab=test2
Sys.time()



if(exists("RefSeuils"))
{
  #ETAPE 1 - formattage des tables et de leurs attributs
  #merge avec esp?ce pour tri selon seuil
  #simplifie la table groupe pour ne pas alourdir la grosse table Data...
  GroupSimpl=data.frame(espece=GroupList$Esp,nom=GroupList$`Scientific name`
                        ,groupe=GroupList$Group)
  GroupRef=merge(GroupSimpl,RefSeuils,by.x="espece",by.y="Espece")
  
  
  DataLPG=merge(DataLP,GroupRef,by="espece")
  test=match(DataLP$espece,GroupRef$espece)
  SpManquante=subset(DataLP,is.na(test))
  table(SpManquante$espece)
  #rm(DataLP)
}else{
  GroupSimpl=data.frame(espece=GroupList$Esp,nom=GroupList$`Scientific name`
                        ,groupe=GroupList$Group)
  
  DataLPG=merge(DataLP,GroupSimpl,by="espece")
}

if(SeuilFiable=="")
{
  DataFiable=DataLPG
}else{
  if(is.numeric(SeuilFiable))
  {
    DataFiable=subset(DataLPG,DataLPG$probabilite>SeuilFiable/100)
  }else{
    ColS=match(SeuilFiable,colnames(DataLPG))
    Fiable=(DataLPG$probabilite>as.data.frame(DataLPG)[,ColS])
    table(Fiable,DataLPG$espece)
    
    DataFiable=subset(DataLPG,Fiable)
    #rm(DataLPG)
    #test=DataFiable[1:100000,]
  }
}

if(Sectorized)
{
Sys.time()
DataRP_ActTron=aggregate(DataFiable$donnee
                         ,by=list(DataFiable$participation
                                  ,DataFiable$Session
                                  ,DataFiable$Secteur
                                  ,DataFiable$Datamicro
                                  ,DataFiable$espece
                         )
                         ,FUN=length) # 2 min
Sys.time()
Sys.time()
DataRP_MaxProb=aggregate(DataFiable$probabilite
                         ,by=list(DataFiable$participation
                                  ,DataFiable$Session
                                  ,DataFiable$Secteur
                                  ,DataFiable$Datamicro
                                  ,DataFiable$espece
                         )
                         ,FUN=max) # 2 min
Sys.time()


DataRP_TimeTron=aggregate(DataLPG$TimeTron
                          ,by=list(DataLPG$participation
                                   ,DataLPG$Session
                          )
                          ,FUN=max)
Sys.time()

DataRP_SpTron=merge(DataRP_ActTron,DataRP_TimeTron
                    ,by=c("Group.1","Group.2"))


colnames(DataRP_SpTron)=c("participation","Tron","Secteur","num_micro"
                          ,"espece"
                          ,"nb_contacts","temps_enr")

DataRP_SpTron=merge(DataRP_SpTron,Q90PipDur,by.x=c("participation","num_micro")
             ,by.y=c("Group.1","Group.2"),all.x=T)
DataRP_SpTron$x[is.na(DataRP_SpTron$x)]=0
colnames(DataRP_SpTron)[ncol(DataRP_SpTron)]="IndiceDurPip"
DataRP_SpTron=merge(DataRP_SpTron,Q90PipProb,by.x=c("participation","num_micro")
             ,by.y=c("Group.1","Group.2"),all.x=T)
DataRP_SpTron$x[is.na(DataRP_SpTron$x)]=0
colnames(DataRP_SpTron)[ncol(DataRP_SpTron)]="IndiceProbPip"
DataRP_SpTron=merge(DataRP_SpTron,DataRP_MaxProb,by.x=c("participation","Tron","Secteur","num_micro"
                                                        ,"espece")
                    ,by.y=c("Group.1","Group.2","Group.3","Group.4"
                            ,"Group.5"),all.x=T)
colnames(DataRP_SpTron)[ncol(DataRP_SpTron)]="score_max"

fwrite(DataRP_SpTron,paste0("DataRP_SpSecteur_",SeuilFiable,".csv"))

}else{
  Sys.time()
  DataRP_ActTron=aggregate(DataFiable$donnee
                           ,by=list(DataFiable$participation
                                    ,DataFiable$Session
                                    ,DataFiable$Datamicro
                                    ,DataFiable$espece
                           )
                           ,FUN=length) # 2 min
  Sys.time()
  DataRP_TimeTron=aggregate(DataLPG$TimeTron
                            ,by=list(DataLPG$participation
                                     ,DataLPG$Session
                            )
                            ,FUN=max)
  Sys.time()
  
  DataRP_SpTron=merge(DataRP_ActTron,DataRP_TimeTron
                      ,by=c("Group.1","Group.2"))
  
  
  colnames(DataRP_SpTron)=c("participation","Tron","num_micro","espece"
                            ,"nb_contacts","temps_enr")
  
  DataRP_SpTron=merge(DataRP_SpTron,Q90PipDur,by.x=c("participation","num_micro")
                      ,by.y=c("Group.1","Group.2"),all.x=T)
  DataRP_SpTron$x[is.na(DataRP_SpTron$x)]=0
  colnames(DataRP_SpTron)[ncol(DataRP_SpTron)]="IndiceDurPip"
  DataRP_SpTron=merge(DataRP_SpTron,Q90PipProb,by.x=c("participation","num_micro")
                      ,by.y=c("Group.1","Group.2"),all.x=T)
  DataRP_SpTron$x[is.na(DataRP_SpTron$x)]=0
  colnames(DataRP_SpTron)[ncol(DataRP_SpTron)]="IndiceProbPip"
  
  fwrite(DataRP_SpTron,paste0("DataRP_SpTron_",SeuilFiable,".csv"))
  
}
