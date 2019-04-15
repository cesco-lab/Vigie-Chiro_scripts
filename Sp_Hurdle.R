library(data.table)
library(glmmTMB)
library(plyr)
library(beepr)
library(corrplot)
FAct="./VigieChiro/ModPred/Data/SpNuit2_Seuil90_bush-cricket_100.csv"  
FAT="AnomalieTemp.csv"
FGIS="./VigieChiro/GIS/GI_coordWGS84_SpNuit2_Seuil50_DataLP_PF_exportTot_Lat41.45_51.61_Long-5.9_9.73.csv"
FVC="variables_choisies"
TagModel="GLMnonselect_MultiSp_DecOT2_AT81_Jour2_simplifie1_CS_STI_Hurdle"
SpeciesList=fread("SpeciesList.csv")
# Famille
familyMod="truncated_nbinom2"
TraitsClim=fread("C:/Users/Yves Bas/Documents/VigieChiro/Traits/ClimNiche_OccSL_bush-cricket.csv")
# Modèle minimal
#FormulaFix_TtSp="nb_contacts~(Jour+I(Jour^2)+I(Jour^3)+I(Jour^4)+I(Jour^5))*DecOT+(AT81+I(AT81^2))+((AT1+I(AT1^2))+(AT9+I(AT9^2)))+SpBioc12+SpHO1S+SpHO2S+SpHO4S+SpWS_S+SpWC_S"
FormulaY="nb_contacts~1"
FormulaXList=c("(Jour+I(Jour^2)+I(Jour^3))"
               ,"(AT81+I(AT81^2))"
               ,"((AT1+I(AT1^2))+(AT9+I(AT9^2)))"
               ,"(DecOT+I(DecOT^2))"
               ,"(SpBioc12+SpHO1S+SpHO2S+SpHO4S+SpWS_S+SpWC_S)"
               ,"ThAve"
               ,"Jour"
               ,"AT81"
               ,"DecOT")
Interactions=list(c(6,7,8),c(6,7,9),c(6,8,9))
#FormulaRandom=paste0("+(1|site/participation)")
#FormulaRandom="+(1|site)"
FormulaRandom="+(1|espece)+(1|site)"
#FormulaRandom="+(1|espece)"
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
SelSample=1e9 #for testing computing time
ExfiltreGite=F

#pour afficher les milisecondes
op <- options(digits.secs=3)


FormulaFix_TtSp=FormulaY
for (i in 1:length(FormulaXList))
{
  FormulaFix_TtSp=paste(FormulaFix_TtSp,FormulaXList[i],sep="+")
}
if(!is.na(Interactions))
{
  for (i in 1:length(Interactions))
  {
    #    Intemp=paste(FormulaXList[Interactions[[i]][1]]
    #                ,FormulaXList[Interactions[[i]][2]],sep="*")
    Intemp=paste(FormulaXList[Interactions[[i]]],collapse="*")
    
    FormulaFix_TtSp=paste(FormulaFix_TtSp,Intemp,sep="+")
  }
}




SpeciesShort=subset(SpeciesList,select=c("Esp","Scientific name","Group"))

# Variables à sélectionner et à tester en interaction
VarSimple=fread(paste0(FVC,".csv"))$variable


SpNuit=fread(FAct)
AnomalieTemp=fread(FAT)
GIS=fread(FGIS)

if(ExfiltreGite)
{
  Gite=mapply(function(x,y) 
    ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
    ,SiteLoc$commentaire
    ,SiteLoc$localite)
  SiteLoc$SpGite=as.numeric(Gite)
  
  SiteLoc=subset(SiteLoc,SiteLoc$SpGite==0)
}
ListPart=levels(as.factor(SpNuit$participation))

PartPF=subset(Particip,Particip$participation %in% ListPart)
SLP=merge(PartPF,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
SpNuit_SLP=merge(SpNuit,SLP,by="participation")

SpNuit_SLP$Long25=(floor(SpNuit_SLP$longitude*4)/4)+0.125
SpNuit_SLP$Lat25=(floor(SpNuit_SLP$latitude*4)/4)+0.125

SpNuit_SLPA=merge(SpNuit_SLP,AnomalieTemp,by.x=c("Long25","Lat25","Nuit")
                  ,by.y=c("Long25","Lat25","V3"))

boxplot(SpNuit_SLPA$AT81~substr(SpNuit_SLPA$Nuit,1,4))


#Rates=subset(SpNuit_SLP,!(SpNuit_SLP$participation %in% SpNuit_SLPA$participation))
#table(Rates$site)


SpNuit_SLPAG=merge(SpNuit_SLPA,GIS,by.x=c("longitude","latitude")
                   ,by.y=c("Group.1.x","Group.2.x"))

SpNuit_SLPAG=subset(SpNuit_SLPAG,!is.na(SpNuit_SLPAG$SpAltiS))


fwrite(data.frame(variables=names(SpNuit_SLPAG)),"variables_dispo.csv")


# Calcul du VIF (adapté à glmmTMB, sinon il faut adapter v et nam)
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)$cond
  nam <- names(fixef(fit)$cond)
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}


# Pour correction autocorrelation spatiale
#MaDataActiNew$FauxGroupe=rep(1,nrow(MaDataActiNew))
#MaDataActiNew$Coord=numFactor(MaDataActiNew$X,MaDataActiNew$Y)


SpNuit_SLPAGN=as.data.frame(SpNuit_SLPAG)
SpNuit_SLPAGN$Jour=yday(as.Date(SpNuit_SLPAGN$Nuit))
SpNuit_SLPAGN$DecOT=(SpNuit_SLPAGN$SpBioc10+SpNuit_SLPAGN$SpBioc11)/2

SpNuit_SLPAGN=merge(SpNuit_SLPAGN,TraitsClim,by.x="espece",by.y="Species")

SpNuit_Scale=subset(SpNuit_SLPAGN,select=c("groupe","espece","participation","site","num_micro","nb_contacts","Nuit"))


Mean=vector()
Sdev=vector()
VarList=vector()
for (i in 1:length(VarSimple))
{
  if(substr(VarSimple[i],1,1)=="(")
  {
    Terms=tstrsplit(VarSimple[i],split="[+]")
    VarTemp=substr(Terms[[1]],2,nchar(Terms[[1]]))
  }else{
    VarTemp=VarSimple[i]
  }
  VarList=c(VarList,VarTemp)
  Vinit=(SpNuit_SLPAGN)[,VarTemp]
  Vscale=scale(Vinit)
  Mean=c(Mean,mean(Vinit))
  Sdev=c(Sdev,sd(Vinit))
  SpNuit_Scale=cbind(SpNuit_Scale,Vscale)
  names(SpNuit_Scale)[ncol(SpNuit_Scale)]=VarTemp
  if(i%%10==1){print(paste(i,Sys.time()))}
}
forBackTransform=data.frame(cbind(VarList,Mean,Sdev))
fwrite(forBackTransform,paste0("forBackTransform_",FVC,".csv"))

ColNumTest=unlist(lapply(SpNuit_Scale[1,],FUN=function(x) is.numeric(x)))
ColNum=subset(names(SpNuit_Scale),ColNumTest)
SpNuit_ColNum=subset(SpNuit_Scale,select=ColNum)
SpNuit_ColNum$DecOT=(SpNuit_ColNum$SpBioc10+SpNuit_ColNum$SpBioc11)/2
SpNuit_ColNum$num_micro=NULL
SpNuit_ColNum$SpBioc10=NULL
SpNuit_ColNum$SpBioc11=NULL
MatCor=cor(SpNuit_ColNum)
corrplot(MatCor)

"
SpNuit_Scale$Jour_2=SpNuit_Scale$Jour^2
SpNuit_Scale$Jour_3=SpNuit_Scale$Jour^3
SpNuit_Scale$Jour_4=SpNuit_Scale$Jour^4
SpNuit_Scale$Jour_5=SpNuit_Scale$Jour^5
SpNuit_Scale$AT1_2=SpNuit_Scale$AT1^2
SpNuit_Scale$AT3_2=SpNuit_Scale$AT3^2
SpNuit_Scale$AT9_2=SpNuit_Scale$AT9^2
SpNuit_Scale$AT27_2=SpNuit_Scale$AT27^2
SpNuit_Scale$AT81_2=SpNuit_Scale$AT81^2
"

Formula=as.formula(paste0(FormulaFix_TtSp
                          ,FormulaRandom))
if(SelSample<nrow(SpNuit_Scale))
{
  SpNuit_Sample=SpNuit_Scale[sample.int(nrow(SpNuit_Scale),SelSample),]
}else{
  SpNuit_Sample=SpNuit_Scale
}
Sys.time()
ModSp=glmmTMB(Formula,data=SpNuit_Sample, family=familyMod,ziformula=~.)  #37 min
Sys.time()
beep()
Res=residuals(ModSp)

Estimates=as.data.frame(coef(summary(ModSp))$cond)
Estimates=cbind(term=row.names(Estimates),Estimates)
save(ModSp,file=paste0("./VigieChiro/GLMs/",TagModel,".glm"))
VIFMod=c(1,vif.mer(ModSp))
Estimates$VIF=VIFMod
Suffix=tstrsplit(basename(FAct),split="[.]")[[1]]

fwrite(Estimates,paste0("./VigieChiro/GLMs/Summaries/",TagModel,"_",Suffix,"_Coefs.csv"),sep=";")

fwrite(as.list(FormulaFix_TtSp),paste0("./VigieChiro/GLMs/logs/",substr(Sys.time(),1,13),".log"))

fwrite(as.data.frame(Res),paste0("./VigieChiro/GLMs/",TagModel,"_",Suffix,"_Res.csv"))
