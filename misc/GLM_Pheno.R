source("./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/functions/GLMs/f_Sp_GLM_short.R")
library(data.table)
library(beepr)

SpeciesToExclude=c("Cyrscu") #too high FP (see DataGroupFP.csv) - 15%FP threshold
#SpeciesToExclude=c("Cyrscu","Confus","Roeroe") #too high FP (see DataGroupFP.csv) - 15%FP threshold
#SpeciesToExclude=c("Cyrscu","Confus","Roeroe","Eupsp","Plaalb","Rhasp","Plaint","Plaaff")
#SpeciesToExclude=""
#too high FP (see DataGroupFP.csv) - 10%FP threshold
DataPF=fread("C:/wamp64/www/SpNuit2_5090_DataLP_PF_exportTot.csv")
SpeciesList=fread("SpeciesList.csv")
#TO UPDATE (8% missing data)
Anom=fread("./VigieChiro/Weather/SLAll_NETCDF.csv") #or SLAll_W or SLAll_NETCDF
GI=fread("C:/wamp64/www/GI_sites_localites.csv") #
TagModel="GLM_PhenoTM_5090_PF20"
familyMod="nbinom2"
TraitsClim=fread("C:/Users/Yves Bas/Documents/VigieChiro/Traits/ClimNiche_OccSL_bush-cricket.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
# Modèle minimal
#FormulaFix_TtSp="nb_contacts~(Jour+I(Jour^2)+I(Jour^3)+I(Jour^4)+I(Jour^5))*DecOT+(AT81+I(AT81^2))+((AT1+I(AT1^2))+(AT9+I(AT9^2)))+SpBioc12+SpHO1S+SpHO2S+SpHO4S+SpWS_S+SpWC_S"
FormulaXList=c("Jour+I(Jour^2)+I(Jour^3)"
  #"(Jour+I(Jour^2)+I(Jour^3))"
               #,"(AT100+I(AT100^2))"
  ,"AT100+I(AT100^2)"
  ,"AT1+I(AT1^2)"
  #,"((AT1+I(AT1^2))+(AT10+I(AT10^2)))"
              # ,"(SpBioC1+I(SpBioC1^2))"
  ,"SpBioC1+I(SpBioC1^2)"
                 ,"ThAve"
               ,"Jour"
               ,"AT100"
               ,"SpBioC1"
  #,"CroplandS","WoodlandS","UrbanS"
  ,"AT10+I(AT10^2)"
  #,"SpBioC12"
  
  )
#Interactions=list(c(5,6,7),c(5,6,8),c(5,7,8),c(5,4)) #for thermal range
Interactions=list(c(5,6,7),c(5,6,8),c(5,7,8)) #for thermal average

#FormulaRandom=paste0("+(1|site/participation)")
#FormulaRandom="+(1|site)"
FormulaRandom="+(1|espece)+(1|sitepoint)"

#Particip=fread("C:/wamp64/www/p_export.csv")
#SiteLoc=fread("C:/wamp64/www/sites_localites.txt")

BSList=subset(SpeciesList,SpeciesList$Group=="bush-cricket")
DataBS=subset(DataPF,DataPF$espece %in% BSList$Esp)
NbDataPerSp=aggregate(DataBS$nb_contacts,by=list(DataBS$espece),length)
SpSel=subset(NbDataPerSp,NbDataPerSp$x>100)
SpSel=subset(SpSel,!(SpSel$Group.1 %in% SpeciesToExclude))


DataBS=subset(DataPF,DataPF$espece %in% SpSel$Group.1)
DataBS=subset(DataBS,DataBS$num_micro==0)
ListNuit=unique(subset(DataBS,select=c("participation","Nuit")))
ListNuit$formerge=1
ListSp=unique(subset(DataBS,select=c("espece")))
ListSp$formerge=1
ListNS=merge(ListNuit,ListSp,by="formerge",allow.cartesian = T)  
DataBS_w0=merge(DataBS,ListNS,by=c("participation","Nuit","espece"),all.y=T)
DataBS_w0$nb_contacts[is.na(DataBS_w0$nb_contacts)]=0
DataBS_w0$Jour=yday(DataBS_w0$Nuit)



DataBSA=merge(DataBS_w0,Anom,by=c("participation","Nuit"))
DataBSAP=merge(DataBSA,Particip,by=c("participation"))
DataBSAPS=merge(DataBSAP,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
DataBSAG=merge(DataBSAPS,GI,by.x=c("longitude.y","latitude.y")
                 ,by.y=c("longitude","latitude"))
DataBSAG=merge(DataBSAG,TraitsClim,by.x="espece",by.y="Species")


DataBSAG$AT1=(DataBSAG$TX_0_0+DataBSAG$TN_1_1)/2
hist(DataBSAG$AT1)
DataBSAG$AT10=(DataBSAG$'TX_-9_0'+DataBSAG$'TN_-8_1')/2
hist(DataBSAG$AT10)
DataBSAG$AT100=(DataBSAG$'TX_-99_0'+DataBSAG$'TN_-98_1')/2
hist(DataBSAG$AT100,breaks=50)

DataBSAG=subset(DataBSAG,!is.na(DataBSAG$AT100))
DataBSAG=subset(DataBSAG,!is.na(DataBSAG$AT1))

DataBSAG$sitepoint=paste(DataBSAG$site,DataBSAG$point)

DataBSAG$UrbanS=DataBSAG$SpHO1S+DataBSAG$SpHO2S+DataBSAG$SpHO3S
DataBSAG$CroplandS=DataBSAG$SpHO5S+DataBSAG$SpHO6S+DataBSAG$SpHO7S+DataBSAG$SpHO8S+DataBSAG$SpHO9S+DataBSAG$SpHO10S+DataBSAG$SpHO11S+DataBSAG$SpHO12S+DataBSAG$SpHO14S+  DataBSAG$SpHO15S
DataBSAG$GrasslandS=DataBSAG$SpHO13S+DataBSAG$SpHO18S+DataBSAG$SpHO19S
DataBSAG$WoodlandS=DataBSAG$SpHO16S+DataBSAG$SpHO17S



NbOcc=aggregate(DataBSAG$nb_contacts,by=list(DataBSAG$espece)
                ,FUN=function(x) (sum(x>0)))
SpeciesOrder=NbOcc$Group.1[order(NbOcc$x,decreasing=T)]

#for (i in 1:length(SpeciesOrder))
#{
  
#DataBSAGi=subset(DataBSAG,DataBSAG$espece==SpeciesOrder[i])

Sp_GLM_short(dataFile=""
             ,
             varInterest="nb_contacts"
             ,
             listEffects=FormulaXList
             ,
             interactions=Interactions
             ,
             formulaRandom=FormulaRandom
             ,
             selSample=1e10
             ,
             tagModel=TagModel
             ,
             family="nbinom2"
             ,
             asfactor=NA
             ,
             data=as.data.frame(DataBSAG)
             ,
             repout=NULL
             ,
             checkRepout=TRUE
             ,
             saveFig=TRUE
             ,
             output=F
             ,
             doBeep=F
             ,
             printFormula=TRUE
             ,
             woInt=F
)
beep()
#}