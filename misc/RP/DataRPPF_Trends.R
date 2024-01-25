library(data.table)
library(glmmTMB)
library(plyr)
library(beepr)
library(corrplot)
FAll="C:/Users/yvesb/Documents/mnhn/Mathilde/V1_nb_checkvar_scaled/data/forGLM2203_SR45.csv"  # Variables à sélectionner et à tester en interaction
VarAbondance="nb_contacts"
VarSimple=c("year","poly(julian,2)","expansion_direct","temps_enr","protocole")
##Interactions=list(c(6,7,8),c(6,7,9),c(6,8,9),c(6,4))
Interactions=NA
FormulaRandom="+(1 | site)+(1 | mat)"
selSample=1e5
#tagModel="test"
family="nbinom2"
asfactor=NA
repout="C:/Users/yvesb/Documents/mnhn/Mathilde/GLMM_test2/"
checkRepout=TRUE
saveFig=TRUE
output=F
doBeep=TRUE
printFormula=TRUE
woInt=F



if(!is.na(asfactor)) {
  VarAbondance <- paste0(VarAbondance,ifelse(VarAbondance %in% asfactor,"_as_factor",""))
  VarSimple <- paste0(VarSimple,ifelse(VarSimple %in% asfactor,"_as_factor",""))
  if(!is.na(Interactions))
    for(l in 1:length(Interactions))
      Interactions[[l]] <- paste0(Interactions[[l]],ifelse(Interactions[[l]] %in% asfactor,"_as_factor",""))
}

##FormulaRandom="+(1|espece)+(1|site)"
##FormulaRandom="+(1|espece)"
SelSample=selSample #for testing computing time
##variables à rajouter : bioclim1 et 11, type de détecteur

## Famille
familyMod=family
## Modèle minimal
##FormulaFix_TtSp="nb_contacts~(Jour+I(Jour^2)+I(Jour^3)+I(Jour^4)+I(Jour^5))*DecOT+(AT81+I(AT81^2))+((AT1+I(AT1^2))+(AT9+I(AT9^2)))+SpBioc12+SpHO1S+SpHO2S+SpHO4S+SpWS_S+SpWC_S"
if(woInt)
{
  FormulaY=paste0(VarAbondance,"~-1")
}else{
  FormulaY=paste0(VarAbondance,"~1")
  
}
FormulaXList=VarSimple

##pour afficher les milisecondes
op <- options(digits.secs=3)



## preparaton des repertoires de sortie

if(is.null(repout)) repout <- "./VigieChiro/GLMs/"
repoutSummary <- paste0(repout,"Summaries/")
repoutLogs <- paste0(repout,"logs/")
repoutBackTransform <- paste0(repout,"forBackTransform/")
repoutModel <- paste0(repout,"Model/")
if(saveFig)repfig <- paste0(repout,"forBackTransform/")

if(checkRepout) {
  dir.create(repout,showWarnings=FALSE)
  dir.create(repoutSummary,showWarnings=FALSE)
  dir.create(repoutLogs,showWarnings=FALSE)
  dir.create(repoutBackTransform,showWarnings=FALSE)
  dir.create(repoutModel,showWarnings=FALSE)
  if(saveFig) dir.create(repfig,showWarnings=FALSE)
}


FormulaFix_TtSp=FormulaY
for (i in 1:length(FormulaXList))
{
  FormulaFix_TtSp=paste(FormulaFix_TtSp,FormulaXList[i],sep="+")
}
if(!is.na(Interactions[1]))
{
  for (i in 1:length(Interactions))
  {
    ##    Intemp=paste(FormulaXList[Interactions[[i]][1]]
    ##                ,FormulaXList[Interactions[[i]][2]],sep="*")
    Intemp=paste(FormulaXList[Interactions[[i]]],collapse="*")
    
    FormulaFix_TtSp=paste(FormulaFix_TtSp,Intemp,sep="+")
  }
}


SpAll=fread(FAll)
ListSp=unique(SpAll$species)

for (h in 1:length(ListSp)){
  SpNuit=subset(SpAll,SpAll$species==ListSp[h])
  TagModel=ListSp[h]
  
  if(!is.na(asfactor))
  {
    SpNuit=as.data.frame(SpNuit)
    for (i in 1:length(asfactor))
    {
      test <- match(asfactor[i],names(SpNuit))
      newcolname <- paste0(colnames(SpNuit)[test],"_as_factor")
      SpNuit[,newcolname] <-  as.factor(SpNuit[,test])
    }
    SpNuit=as.data.table(SpNuit)
    
  }
  
  
  
  
  ##compute summaries of activity
  ColA=match(VarAbondance,names(SpNuit))
  Ab=as.data.frame(SpNuit)[,ColA]
  
  SpNuitwoNA=subset(SpNuit,!is.na(Ab))
  AbwoNA=subset(Ab,!is.na(Ab))
  
  
  
  SpA1=aggregate(AbwoNA,by=list(SpNuitwoNA$espece),FUN=mean)
  
  if(length(unique(SpNuit$espece))>1) {
    barplot(SpA1$x,names.arg=SpA1$Group.1,las=2,cex.names=0.6)
  }
  SpPos=subset(SpNuitwoNA,AbwoNA>0)
  
  
  AbPos=subset(Ab,Ab>0)
  
  cat("\n Number of occurence:",(length(AbPos)),"\n")
  if(length(AbPos)<=length(VarSimple))
  {
    stop(paste(FAll,": too few positive data to fit model"),"\n")
  }
  
  
  if(length(unique(SpNuit$espece))>1) {
    SpOcc=aggregate(AbPos,by=list(SpPos$espece),FUN=length)
    barplot(SpOcc$x,names.arg=SpOcc$Group.1,las=2,cex.names=0.6)
    
    SpAbIfP=aggregate(AbPos,by=list(SpPos$espece),FUN=mean)
    barplot(SpAbIfP$x,names.arg=SpAbIfP$Group.1,las=2,cex.names=0.6)
  }
  ## Calcul du VIF (adapté à glmmTMB, sinon il faut adapter v et nam)
  ## adapted from rms::vif
  vif.mer <- function (fit) {
    
    ## Variance-Covariance Matrix
    v <- vcov(fit)$cond
    nam <- names(fixef(fit)$cond)
    
    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
      v <- v[-(1:ns), -(1:ns), drop = FALSE]
      nam <- nam[-(1:ns)]
    }
    
    ## squarre root of the diagonal matrix (of the variance-covariance matrix)
    # !! doesn't work if diag(v) product negative values !!
    d <- diag(v)^0.5
    if(any(is.na(d))) {
      cat("! the diagonal matrice of the variance covariance matrix produce at least one negative value !\n")
      cat("  -> the VIFs values are not assessed\n\n")
    }
    
    ## variance-covariance matrix on outer product of d
    d <- v/(d %o% d)
    ## inversing d
    d <- solve(d)
    ## and return the diag of d
    v <- diag(d)
    names(v) <- nam
    v
  }
  
  
  ## Pour correction autocorrelation spatiale
  ##MaDataActiNew$FauxGroupe=rep(1,nrow(MaDataActiNew))
  ##MaDataActiNew$Coord=numFactor(MaDataActiNew$X,MaDataActiNew$Y)
  
  
  SpNuit_SLPAGN=as.data.frame(SpNuitwoNA)
  
  #OtherVariables=subset(names(SpNuit),!(names(SpNuit) %in% VarSimple))
  
  SpNuit_Scale=SpNuit_SLPAGN
  
  
  cat("Variable transformation to reduced centered variable\n")
  cat("   ",length(VarSimple),"variable(s) to transform")
  start <- Sys.time() ## heure de demarage
  cat("  ",format(start, "%d-%m-%Y %HH%M"),"  ... \n ")
  
  
  Mean=vector()
  Sdev=vector()
  VarList=vector()
  for (i in 1:length(VarSimple))
  {
    cat(i,"")
    if(grepl("poly",VarSimple[i]))
    {
      Var=gsub("poly","",VarSimple[i])
      Terms=tstrsplit(Var,split=",")
      VarTemp=substr(Terms[[1]],2,nchar(Terms[[1]]))
      #VarTemp=tstrsplit(VarSimple[i],split="\\+")[[1]]
    }else{
      VarTemp=VarSimple[i]
    }
    VarList=c(VarList,VarTemp)
    Vinit=SpNuit_SLPAGN[,VarTemp]
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
    SpNuit_Scale[,VarTemp]=NULL
    SpNuit_Scale=cbind(SpNuit_Scale,Vscale)
    names(SpNuit_Scale)[ncol(SpNuit_Scale)]=VarTemp
    if(i%%10==1 & i != 1){
      timeAvancement <- Sys.time() ## heure de d'avancement
      timeAvancement <- format(timeAvancement, "%d-%m-%Y %HH%M")
      cat("\n       ",timeAvancement,"  ... \n")}
    
    
    
    
  }
  
  
  cat("DONE\n")
  end <- Sys.time() ## heure de fin
  diff <- end-start
  diff <- paste(round(diff,1),units(diff))
  cat("      ", format(end, "%d-%m-%Y %HH%M")," -> ",diff,"\n\n")
  
  forBackTransform=data.frame(cbind(VarList,Mean,Sdev))
  forBackTransform=unique(forBackTransform,by="VarList")
  
  csvfile <- paste0(repoutBackTransform,TagModel,".csv")
  cat("  --> [CSV]:",csvfile)
  fwrite(forBackTransform,csvfile)
  cat("    DONE !\n")
  
  ColNumTest=unlist(lapply(SpNuit_Scale[1,],FUN=function(x) is.numeric(x)))
  ColNum=subset(names(SpNuit_Scale),ColNumTest)
  SpNuit_ColNum=subset(SpNuit_Scale,select=ColNum)
  #MatCor=cor(SpNuit_ColNum)
  
  
  #if(saveFig){
  #  pngfile <- paste0(repfig,"corrplot","_",tagModel,".png")
  # cat("  --> [PNG]:",pngfile)
  #png(pngfile)
  #}
  #corrplot(MatCor)
  #if(saveFig){
  # dev.off()
  #cat("    DONE !\n")
  
  #}
  
  Formula=as.formula(paste0(FormulaFix_TtSp
                            ,FormulaRandom))
  
  if(SelSample<nrow(SpNuit_Scale))
  {
    SpNuit_Sample=SpNuit_Scale[sample.int(nrow(SpNuit_Scale),SelSample),]
  }else{
    SpNuit_Sample=SpNuit_Scale
  }
  
  cat("\nModel glmmTMB\n")
  start <- Sys.time() ## heure de demarage
  cat("  ",format(start, "%d-%m-%Y %HH%M"),"  ...  ")
  
  if(printFormula) cat("\nglmmTMB(Formula= ",as.character(Formula)[2]," ",as.character(Formula)[1]," ",as.character(Formula)[3],", family= ",familyMod,")\n",sep="")
  
  
  print(Sys.time())
  ModSp=glmmTMB(Formula,data=SpNuit_Sample, family=familyMod)  #37 min
  print(Sys.time())
    if(doBeep) beep()
  summary(ModSp)
  
  end <- Sys.time() ## heure de demarage
  diff <- end-start
  diff <- paste(round(diff,1),units(diff))
  cat(format(end, "%d-%m-%Y %HH%M")," -> ",diff,"\n\n")
  
  
  Res=residuals(ModSp)
  SpNuit_Sample$Res=Res
  
  Estimates=as.data.frame(coef(summary(ModSp))$cond)
  
  term <- row.names(Estimates)
  Estimates=cbind(term=term,Estimates)
  
  colnames(forBackTransform) <- c("term","bt_mean","bt_sd")
  forBackTransform$bt_sd <- as.numeric(as.character(forBackTransform$bt_sd))
  forBackTransform$bt_mean <- as.numeric(as.character(forBackTransform$bt_mean))
  
  Estimates <- merge(Estimates,forBackTransform,by="term",all.x=TRUE)
  
  #PROBLEM: link is not log for binomial but logit, rather extract link function in glm object itself
  if(family %in% c("nbinom","nbinom2","poisson","binomial")) Estimates$coef_raw <- exp(Estimates$Estimate) else Estimates$coef_raw <- Estimates$Estimate
  
  Estimates$coef <- ifelse(is.na(Estimates$bt_sd),Estimates$coef_raw,Estimates$coef_raw^(1/Estimates$bt_sd)) #backtransforme les coefficients de variables traitées comme numériques (pas de modif sinon)
  
  
  mdIC <- as.data.frame(confint(ModSp)[,1:2])
  rownames(mdIC) <- gsub("cond.","",rownames(mdIC))
  colnames(mdIC) <- c("ICinf","ICsup")
  mdIC <- mdIC[as.character(Estimates$term),]
  
  Estimates <- data.frame(Estimates,mdIC)
  Estimates$ICinf <- ifelse(is.na(Estimates$bt_sd),exp(Estimates$ICinf),exp(Estimates$ICinf)^(1/Estimates$bt_sd))
  Estimates$ICsup <- ifelse(is.na(Estimates$bt_sd),exp(Estimates$ICsup),exp(Estimates$ICsup)^(1/Estimates$bt_sd))
  
  
  glmfile <- paste0(repoutModel,TagModel,".glm")
  cat("  --> [GLM]:",glmfile)
  save(ModSp,file=glmfile)
  cat("    DONE !\n")
  
  if(woInt)
  {
    VIFMod=vif.mer(ModSp)
  }else{
    VIFMod=c(1,vif.mer(ModSp))
    
  }
  Estimates$VIF=VIFMod
  
  if(FAll!="")
  {
    Suffix=tstrsplit(basename(as.character(FAll)),split="[.]")[[1]]
  }else{
    Suffix=tagModel
  }
  
  csvfile <- paste0(repoutSummary,TagModel,"_",Suffix,"_Coefs.csv")
  cat("  --> [CSV]:",csvfile)
  fwrite(Estimates,csvfile,sep=";")
  cat("    DONE !\n")
  
  csvfile <- paste0(repoutLogs,substr(Sys.time(),1,13),".log")
  cat("  --> [CSV]:",csvfile)
  fwrite(as.list(FormulaFix_TtSp),csvfile)
  cat("    DONE !\n")
  
  csvfile <- paste0(repoutModel,TagModel,"_",Suffix,"_Res.csv")
  cat("  --> [CSV]:",csvfile)
  fwrite(SpNuit_Sample,csvfile)
  cat("    DONE !\n")
  
  if(output) return(list(ModSp,Estimates,SpNuit_Sample))
}



##for test

#' a relatively generic function to fit multiple regression with glmmTMB, scaling numeric variables, supporting interactions and conversion of numeric variables to factors
#' @param dataFile data file location
#' @param varInterest the name of the variable to be regressed (a character value)
#' @param listEffects the names of the explanatory variables (a character vector). Polynomial effects should be written as "poly(x,n)"
#' @param interactions a list of numeric vectors giving the position of the explanatory variables composing interactions (TO DO : adapt script to give variable names instead), default to NA (= no interactions)
#' @param formulaRandom the random part of the formula starting by "+" (default to ="+1" = no random effects)
#' @param selSample numeric, to downsample data (for testing)
#' @param tagModel a character tag identifying model outputs
#' @param family distribution family of varInterest (default to "nbinom2", probably the better choice for abundance data)
#' @param asfactor a character vector giving the numeric variables to be treated as factor in the modelling, default to NA
#' @param data by default the function load the file from dataFile, it's also possible to declare the data.frame
#' @param repout le path for the output
#' @param checkRepout TRUE to verifiy the presence of repout and create it (and some sub-repertories) if no exist
#' @param saveFig TRUE to save the figures
#' @param output to get a multiple object from the function, by default FALSE
#' @param doBeep TRUE to do a beep after model fitting
#' @param printFormula TRUE to print the formula
#' @param woInt TRUE to suppress the intercept in the model formula
#' @return write 5 files: (1) a .glm to save model fit in R format; (2) a "XXX_coefs.csv" table giving estimates and vif coefficients of the model; (3) a "XXX.log" to keep track of the formula of the model; (4) a "XXX_Res.csv" a table giving the residuals value; (5) a "forBackTransform_XXX.csv" table giving the mean and standard deviation value of numeric explanatory variables, to allow back transformation to real values when predicting
#' @example see at the end of this code
#' @author Yves Bas, Charlotte Roemer and Romain Lorrilliere
