library(data.table)
library(glmmTMB)
library(plyr)
library(beepr)
library(corrplot)
FAct="SpNuit2_Seuil50_DataLP_PF_exportTot.csv"  
FAT="AnomalieTemp.csv"
FGIS="./VigieChiro/GIS/GI_coordWGS84_SpNuit2_Seuil50_DataLP_PF_exportTot_Lat41.45_51.61_Long-5.9_9.73.csv"
FVC="variables_choisies.csv"

# Modèle minimal
FormulaFix_TtSp=paste0("nb_contacts~1")
# Variables à sélectionner et à tester en interaction
VarSimple=fread(FVC)$variable
# Parmi ces variables, lesquelles ne doivent pas être testées en interaction avec d'autres
VarDispoSansInter=""
# Effet aléatoire (+ autocorrelation spatiale si nécessaire)
#FormulaRandom=paste0("+(1|site/participation)")
FormulaRandom=""

# Famille
familyMod="nbinom2"




SpNuit=fread(FAct)
AnomalieTemp=fread(FAT)
GIS=fread(FGIS)
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")

ListPart=levels(as.factor(SpNuit$participation))

PartPF=subset(Particip,Particip$participation %in% ListPart)
SLP=merge(PartPF,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
SpNuit_SLP=merge(SpNuit,SLP,by="participation")

SpNuit_SLP$Long25=(floor(SpNuit_SLP$longitude*4)/4)+0.125
SpNuit_SLP$Lat25=(floor(SpNuit_SLP$latitude*4)/4)+0.125

SpNuit_SLPA=merge(SpNuit_SLP,AnomalieTemp,by.x=c("Long25","Lat25","Nuit")
                  ,by.y=c("Long25","Lat25","V3"))

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


PowerIntLog=c(1/c(10:2),c(1:10)) #for power function
#PowerIntLog=c(-10:10) #for log function
SpNuit_SLPAGN=as.data.frame(SpNuit_SLPAG)
SpNuit_SLPAGN$Jour=yday(as.Date(SpNuit_SLPAGN$Nuit))



#norm & scale
Power=vector()

for (i in 1:length(VarSimple))
{
  Vinit=(SpNuit_SLPAGN)[,VarSimple[i]]
  if(min(Vinit)!=max(Vinit))
  {
  Vmin=min(Vinit)
  VPos=Vinit-Vmin
  VLog=vector()
  #Stat=vector()
  #PVal=vector()
  M1=vector()
  M2=vector()
  for (j in 1:length(PowerIntLog))
  {
    LogTemp=VPos^PowerIntLog[j]
    #LogTemp=log10(VPos+10^PowerIntLog[j])
  VLog=cbind(VLog,LogTemp)
  #hist(LogTemp,main=PowerIntLog[j])
  #test=ks.test(scale(LogTemp),"pnorm")
  #Stat=c(Stat,test$statistic)
  #PVal=c(PVal,test$p.value)
  LogTS=scale(LogTemp)
  #if(quantile(LogTS,0.75)!=quantile(LogTS,0.25))
  #{
  #M1=c(M1,abs(quantile(LogTS,0.75)+quantile(LogTS,0.25))/(quantile(LogTS,0.75)-quantile(LogTS,0.25)))
  #}else{
  #  M1=c(M1,999)
  #}
  M2=c(M2,abs(max(LogTS)+min(LogTS))/(max(LogTS)-min(LogTS)))
  #print(j)
  #print(summary(LogTemp))
  #qqnorm(LogTemp,main=j)
  }
  
  Power=c(Power,PowerIntLog[which.min(M2)])
  
  }else{
    Power=c(Power,999)
  }
print(VarSimple[i])
print(Power[i])
print(Sys.time())
}
  
    #if(sd(M1)!=0){
  #  M3=scale(M1)+scale(M2)
  #}else{
  #  M3=M2
  #}
  
SpNuit_Scale=subset(SpNuit_SLPAGN,select=c("groupe","espece","participation","site","num_micro","nb_contacts","Nuit"))


Mean=vector()
Sdev=vector()

for (i in 1:length(VarSimple))
{
  if(substr(VarSimple[i],1,1)=="(")
  {
    Terms=tstrsplit(VarSimple[i],split="[+]")
    VarTemp=substr(Terms[[1]],2,nchar(Terms[[1]]))
  }else{
    VarTemp=VarSimple[i]
    }
  Vinit=(SpNuit_SLPAGN)[,VarTemp]
  Vscale=scale(Vinit)
  Mean=c(Mean,mean(Vinit))
  Sdev=c(Sdev,sd(Vinit))
  SpNuit_Scale=cbind(SpNuit_Scale,Vscale)
  names(SpNuit_Scale)[ncol(SpNuit_Scale)]=VarTemp
  if(i%%10==1){print(paste(i,Sys.time()))}
}


ColNumTest=unlist(lapply(SpNuit_Scale[1,],FUN=function(x) is.numeric(x)))
ColNum=subset(names(SpNuit_Scale),ColNumTest)
SpNuit_ColNum=subset(SpNuit_Scale,select=ColNum)
MatCor=cor(SpNuit_ColNum)
corrplot(MatCor, method="number")


SpNuit_Scale$Jour_2=SpNuit_Scale$Jour^2
SpNuit_Scale$Jour_3=SpNuit_Scale$Jour^3
SpNuit_Scale$Jour_4=SpNuit_Scale$Jour^4
SpNuit_Scale$Jour_5=SpNuit_Scale$Jour^5
SpNuit_Scale$AT1_2=SpNuit_Scale$AT1^2
SpNuit_Scale$AT3_2=SpNuit_Scale$AT3^2
SpNuit_Scale$AT9_2=SpNuit_Scale$AT9^2
SpNuit_Scale$AT27_2=SpNuit_Scale$AT27^2
SpNuit_Scale$AT81_2=SpNuit_Scale$AT81^2



for (j in 1:nlevels(as.factor(SpNuit_Scale$espece)))
{
  print(Sys.time())
  print(levels(as.factor(SpNuit_Scale$espece))[j])
  SpData=subset(SpNuit_Scale
                ,SpNuit_Scale$espece==levels(as.factor(SpNuit_Scale$espece))[j])

  Formula=as.formula(paste0("nb_contacts~(Jour+Jour_2+Jour_3+Jour_4+Jour_5)+((AT1+AT1_2)+(AT9+AT9_2)+(AT81+AT81_2))+SpBioc10+SpBioc11+SpBioc12"
                            ,FormulaRandom))
  
  
  ModSp=glmmTMB(Formula,data=SpData, family=familyMod)  

  save(ModSp,file=paste0("./VigieChiro/GLMs/GLMnonselect_",levels(as.factor(SpNuit_Scale$espece))[j],".glm"))
}



#selection de modèles
FormulaSp=vector()

for (j in 1:nlevels(as.factor(SpNuit_Scale$espece)))
{
  print(levels(as.factor(SpNuit_Scale$espece))[j])
      SpData=subset(SpNuit_Scale
                ,SpNuit_Scale$espece==levels(as.factor(SpNuit_Scale$espece))[j])
  
  FormulaFix=FormulaFix_TtSp
  
    VarDispo=VarSimple
BestVar=vector(length=0)
DeltaAIC=3
i=0
VarSelect=vector()
VarSelect_sansI=vector()
InteractSelect=vector()
FormulaF=vector()
standE=vector()

Formula=as.formula(paste0(as.character(FormulaFix),FormulaRandom))
Sys.time()
AIC_prec=AIC(glmmTMB(Formula,data=SpData, family=familyMod))
Sys.time()

Fixeffects=list()
degree=vector()
logLik=vector()
AICF=vector()


if(length(VarSimple)>0){
  while(DeltaAIC>2)
  {
    i=i+1
    
    VarSelect_sansIsub=setdiff(VarSelect_sansI, VarDispoSansInter)
    if(length(VarSelect_sansIsub)>1)
    {
      InteractPossible=combn(VarSelect_sansIsub,2,FUN=paste, collapse=':')
      VarDispo=c(VarDispo,InteractPossible)
      VarDispo=VarDispo[!VarDispo %in% VarSelect]
      
    }
    
    AIC=vector()
    for (h in 1:(length(VarDispo)))
    {
      FormulaFixTemp=paste0(as.character(FormulaFix),"+",VarDispo[h])
      Formula=as.formula(paste0(as.character(FormulaFixTemp),FormulaRandom))
      ModTemp=glmmTMB(Formula, data=SpData, family=familyMod)
      #if (max(vif.mer(ModTemp))<3){
        AIC[h]=AIC(ModTemp)
        #}else{AIC[h]=AIC_prec+99999999}
      print(paste(h,length(VarDispo),AIC[h],FormulaFixTemp,Sys.time()))
      Fixeffects=c(Fixeffects,list(t(as.data.frame(fixef(ModTemp)$cond))))
      degree=rbind(degree,attr(logLik(ModTemp), "df"))
      logLik=rbind(logLik,logLik(ModTemp)[1])
      AICF=rbind(AICF,AIC[h])
      standE=c(standE,list(summary(ModTemp)$coefficients$cond[,2]))
      FormulaF=rbind(FormulaF,FormulaFixTemp)
    }
    
    for (k in 1:length(AIC)){
      if (is.na(AIC[k])){print(paste(k,"Warning : AIC=NA"))}
    }
    DeltaAIC=AIC_prec-min(AIC,na.rm=T)
    print(paste("Delta AIC =",DeltaAIC,sep=" "))
    if(DeltaAIC>2)
    {
      N=which(AIC==(min(AIC,na.rm=T)))[1]
      BestVar=VarDispo[N]
      VarDispo=VarDispo[!VarDispo %in% VarDispo[N]]
      VarSelect=c(VarSelect,BestVar)
      VarSelect_sansI=subset(VarSelect,grepl(":",VarSelect)==F)
      
      FormulaFix=paste0(as.character(FormulaFix),"+",BestVar)
      AIC_prec=min(AIC,na.rm=T)
    }
  }
}

print(levels(as.factor(SpNuit_Scale$espece))[j])
print(FormulaFix)
FormulaSp=c(FormulaSp,FormulaFix)
beepr::beep(2)
}


