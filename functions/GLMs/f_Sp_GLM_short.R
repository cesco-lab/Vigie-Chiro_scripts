test=F

Sp_GLM_short=function(dataFile,varInterest,listEffects,interactions=NA
                      ,formulaRandom="+1",selSample=1e10,tagModel=""
                      ,family="nbinom2",asfactor=NA)
{
  
  library(data.table)
  library(glmmTMB)
  library(plyr)
  library(beepr)
  library(corrplot)
  FAct=dataFile  # Variables à sélectionner et à tester en interaction
  VarAbondance=varInterest
  VarSimple=listEffects
  #Interactions=list(c(6,7,8),c(6,7,9),c(6,8,9),c(6,4))
  Interactions=interactions
  FormulaRandom=formulaRandom
  #FormulaRandom="+(1|espece)+(1|site)"
  #FormulaRandom="+(1|espece)"
  SelSample=selSample #for testing computing time
  #variables à rajouter : bioclim1 et 11, type de détecteur
  TagModel=tagModel
  # Famille
  familyMod=family
  # Modèle minimal
  #FormulaFix_TtSp="nb_contacts~(Jour+I(Jour^2)+I(Jour^3)+I(Jour^4)+I(Jour^5))*DecOT+(AT81+I(AT81^2))+((AT1+I(AT1^2))+(AT9+I(AT9^2)))+SpBioc12+SpHO1S+SpHO2S+SpHO4S+SpWS_S+SpWC_S"
  FormulaY=paste0(VarAbondance,"~1")
  FormulaXList=VarSimple
  
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
  
  
  
  
  
  
  SpNuit=fread(FAct)
  
  if(!is.na(asfactor))
  {
    SpNuit=as.data.frame(SpNuit)
    for (i in 1:length(asfactor))
    {
      test=match(asfactor[i],names(SpNuit))
      SpNuit[,test]=as.factor(SpNuit[,test])
    }
    SpNuit=as.data.table(SpNuit)
    
  }
  #compute summaries of activity
  ColA=match(VarAbondance,names(SpNuit))
  Ab=as.data.frame(SpNuit)[,ColA]
  
  SpNuitwoNA=subset(SpNuit,!is.na(Ab))
  AbwoNA=subset(Ab,!is.na(Ab))
  
  
  SpA1=aggregate(AbwoNA,by=list(SpNuitwoNA$espece),FUN=mean)
  
    
    barplot(SpA1$x,names.arg=SpA1$Group.1,las=2,cex.names=0.6)
    SpPos=subset(SpNuitwoNA,AbwoNA>0)
    AbPos=subset(Ab,Ab>0)
    print(length(AbPos))
    if(length(AbPos)<=length(VarSimple))
    {
      print(paste(FAct,": too few positive data to fit model"))
    }else
    {
      
    SpOcc=aggregate(AbPos,by=list(SpPos$espece),FUN=length)
    barplot(SpOcc$x,names.arg=SpOcc$Group.1,las=2,cex.names=0.6)
    
    SpAbIfP=aggregate(AbPos,by=list(SpPos$espece),FUN=mean)
    barplot(SpAbIfP$x,names.arg=SpAbIfP$Group.1,las=2,cex.names=0.6)
    
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
    
    
    SpNuit_SLPAGN=as.data.frame(SpNuitwoNA)
    
    OtherVariables=subset(names(SpNuit),!(names(SpNuit) %in% VarSimple))
    
    SpNuit_Scale=subset(SpNuit_SLPAGN,select=OtherVariables)
    
    
    Mean=vector()
    Sdev=vector()
    VarList=vector()
    for (i in 1:length(VarSimple))
    {
      if(substr(VarSimple[i],1,5)=="poly(")
      {
        Var=gsub("poly","",VarSimple[i])
        Terms=tstrsplit(Var,split=",")
        VarTemp=substr(Terms[[1]],2,nchar(Terms[[1]]))
      }else{
        VarTemp=VarSimple[i]
      }
      VarList=c(VarList,VarTemp)
      Vinit=(SpNuit_SLPAGN)[,VarTemp]
      if(is.numeric(Vinit))
      {
        
        Vscale=scale(Vinit)
        Mean=c(Mean,mean(Vinit))
        Sdev=c(Sdev,sd(Vinit))
      }else{
        Vscale=Vinit
        Mean=c(Mean,NA)
        Sdev=c(Sdev,NA)
      }
      SpNuit_Scale=cbind(SpNuit_Scale,Vscale)
      names(SpNuit_Scale)[ncol(SpNuit_Scale)]=VarTemp
      if(i%%10==1){print(paste(i,Sys.time()))}
      
    }
    forBackTransform=data.frame(cbind(VarList,Mean,Sdev))
    fwrite(forBackTransform,paste0("./VigieChiro/GLMs/forBackTransform/forBackTransform_"
                                   ,TagModel,".csv"))
    
    ColNumTest=unlist(lapply(SpNuit_Scale[1,],FUN=function(x) is.numeric(x)))
    ColNum=subset(names(SpNuit_Scale),ColNumTest)
    SpNuit_ColNum=subset(SpNuit_Scale,select=ColNum)
    MatCor=cor(SpNuit_ColNum)
    corrplot(MatCor)
    Formula=as.formula(paste0(FormulaFix_TtSp
                              ,FormulaRandom))
    
    
    if(SelSample<nrow(SpNuit_Scale))
    {
      SpNuit_Sample=SpNuit_Scale[sample.int(nrow(SpNuit_Scale),SelSample),]
    }else{
      SpNuit_Sample=SpNuit_Scale
    }
    Sys.time()
    ModSp=glmmTMB(Formula,data=SpNuit_Sample, family=familyMod)  #37 min
    Sys.time()
    beep()
    Res=residuals(ModSp)
    SpNuit_Sample$Res=Res
    
    Estimates=as.data.frame(coef(summary(ModSp))$cond)
    Estimates=cbind(term=row.names(Estimates),Estimates)
    save(ModSp,file=paste0("./VigieChiro/GLMs/",TagModel,".glm"))
    VIFMod=c(1,vif.mer(ModSp))
    Estimates$VIF=VIFMod
    Suffix=tstrsplit(basename(FAct),split="[.]")[[1]]
    
    fwrite(Estimates,paste0("./VigieChiro/GLMs/Summaries/",TagModel,"_",Suffix,"_Coefs.csv"),sep=";")
    
    fwrite(as.list(FormulaFix_TtSp),paste0("./VigieChiro/GLMs/logs/",substr(Sys.time(),1,13),".log"))
    
    fwrite(SpNuit_Sample,paste0("./VigieChiro/GLMs/",TagModel,"_",Suffix,"_Res.csv"))
  }
}
#for test
if(test)
{
  Sp_GLM_short(
    dataFile="./VigieChiro/DataSp/RPCirw0_50/Pipkuh.csv"
    ,
    varInterest="nb_contacts_strict"
    ,
    listEffects=c("year","poly(julian,2)","sample_cat","nb_Tron_strict"
                  ,"temps_enr_strict","latitude","longitude","expansion_direct"
    )
    ,
    interactions=NA
    ,
    formulaRandom="+(1|site)"
    ,
    selSample=1e10
    ,
    tagModel="GLMalphatest_tendancesFY"
    ,
    family="nbinom2"
    ,
    asfactor="year"
  )
  
}