library(data.table)
library(glmmTMB)
library(ggeffects)
library(ggplot2)
library(moments)
library(Hmisc)
#VarToPredict=c("DecOT","AT81 [-1,0,1]")
VarToPredict=c("DecOT","AT81")
VarNotToPredict="Jour"
#ToPredict=c("DecOT [((-10:10)/10)]","AT81 [-1,0,1]")
LevelsToPredict=c((-30:30)/10)

#VarToPredict="AT81"

GLMPref="GLMnonselect_DecOT2_AT81"
FFBT="forBackTransform_variables_choisies.csv"

SpeciesList=fread("SpeciesList.csv")
SpeciesShort=subset(SpeciesList,select=c("Esp","Scientific name","Group"))
forBackTransform=fread(FFBT)
ListMod=list.files("./VigieChiro/GLMs/",pattern=GLMPref,full.names=T)
VarMatch=match(VarToPredict,forBackTransform$VarList)

ToPredict=VarToPredict
AddPredict=as.character(LevelsToPredict[1])
for (z in 2:length(LevelsToPredict))
{
  AddPredict=paste(AddPredict,LevelsToPredict[z],sep=",")
  
}
ToPredict[1]=paste0(VarToPredict[1]," [",AddPredict,"]")
if(length(ToPredict)==2){ToPredict[2]=paste(ToPredict[2],"[1,0,-1]")}

Species=vector()
Peak=vector()
Skewness=vector()
Kurtosis=vector()
MeanD=vector()
Q10=vector()
Q25=vector()
Q50=vector()
Q75=vector()
Q90=vector()


for (i in 1:length(ListMod))
{
  print(ListMod[i])
  load(ListMod[i])
  Terms=terms(ModSp)
  TermLabels=attr(Terms,"term.labels")
  
  
  TermSelect=vector()
  for(j in 1:length(VarToPredict))
  {
    TermSelect=c(TermSelect,subset(TermLabels,grepl(VarToPredict[j],TermLabels)))
  }
  TermSelect=subset(TermSelect,!grepl(VarNotToPredict,TermSelect))
  TermTarget=(!is.na(match(TermLabels,TermSelect)))
  PVal=coef(summary(ModSp))$cond[,4]
  PVal_woInt=PVal[2:length(PVal)]
  TestVar=subset(PVal_woInt,TermTarget)
  if(sum(is.na(TestVar))<length(TestVar))
  {
    if(min(subset(TestVar,!is.na(TestVar)))<0.05)
    {
      
      ModInfo=tstrsplit(ListMod[i],"_")
      Species=c(Species,substr(ModInfo[[length(ModInfo)]],nchar(ModInfo[[length(ModInfo)]])-9,nchar(ModInfo[[length(ModInfo)]])-4))
      
      # Create predict table
      Sys.time()
      pr1.0 <- ggpredict(ModSp, c(terms = ToPredict),pretty = FALSE)
      Sys.time()
      pr1=pr1.0
      # Backtransform before scaling (utiliser la table crée lors de l'utilisation de la fonction scale)
      pr1$x=pr1$x*forBackTransform$Sdev[VarMatch[1]]+forBackTransform$Mean[VarMatch[1]]
      pr1$predicted=subset(pr1$predicted,!is.na(pr1$x))
      pr1$group=subset(pr1$group,!is.na(pr1$x))
      pr1$x=subset(pr1$x,!is.na(pr1$x))
      #pr1$invgroup=as.factor(-(as.numeric(as.character(pr1$group))))
      
      
      Xmin=quantile(pr1$x,0.05)
      Xmax=quantile(pr1$x,0.95)
      Ymin=quantile(pr1$predicted,0.04)*0.8
      Ymax=quantile(pr1$predicted,0.96)*1.2
      
      
      RawPredict=log(pr1$predicted)
      
      summary(pr1$predicted)
      Peak=c(Peak,pr1$x[which.max(pr1$predicted)])
      skewness(RawPredict)
      skewness(pr1$predicted)
      #skewness(pr1$predicted[150:270])
      
      Skewness=c(Skewness,skewness(pr1$predicted))
      Kurtosis=c(Kurtosis,kurtosis(pr1$predicted))
      MeanD=c(MeanD,weighted.mean(pr1$x,w=pr1$predicted))
      Quantiles=wtd.quantile(pr1$x,weights=pr1$predicted,probs=c(0.1,0.25,0.5,0.75,0.9))
      Q10=c(Q10,Quantiles[1])
      Q25=c(Q25,Quantiles[2])
      Q50=c(Q50,Quantiles[3])
      Q75=c(Q75,Quantiles[4])
      Q90=c(Q90,Quantiles[5])
      pr1$anom=as.numeric(as.character(pr1$group))
      
      pr1$predicted=pmin(pr1$predicted,10000)    
      
      # Plot
      png(filename=paste0(dirname(ListMod[i]),"/Plots/",VarToPredict,"/",basename(ListMod[i]),".png"), res=100)
      #GradColor=scale_color_gradient2(low="blue",mid="green",high="red")
      if(nlevels(pr1$group)>1)
      {
        
        print(ggplot(pr1, aes(x, predicted,fill=group)) +
                geom_line(aes(color = anom),size=1)  +
              scale_color_gradient2(low="blue",mid="green",high="red") +
                geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
                xlab(VarToPredict) + 
               ylab("Acoustic Activity") +
                scale_x_continuous(limits = c(Xmin, Xmax)) +
                scale_y_continuous(limits = c(Ymin, Ymax)) +
                
               ggtitle(ListMod[i]) + 
               scale_fill_discrete(guide=FALSE)+
                theme_bw(base_size = 13)
        )
        
      }else{
        print(ggplot(pr1, aes(x, predicted)) +
                geom_line()  +
                #scale_color_gradient2(low="blue",mid="green",high="red") +
                geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
                xlab(VarToPredict) + 
                ylab("Acoustic Activity") +
                scale_x_continuous(limits = c(Xmin, Xmax)) +
                scale_y_continuous(limits = c(Ymin, Ymax)) +
                
                ggtitle(ListMod[i]) + 
                theme_bw(base_size = 13)
        )
        
      }
      dev.off()
    }
  }
}

Skew=Peak-MeanD
Assymetry=2*Q50-Q75-Q25
DurPeak=Q75-Q25
DurAct=Q90-Q10

SpMetrics=data.frame(cbind(Species
                           ,Peak
                           ,Skewness
                           ,Kurtosis
                           ,MeanD
                           ,Q10
                           ,Q25
                           ,Q50
                           ,Q75
                           ,Q90
                           ,Skew,Assymetry,DurPeak,DurAct))


SpMetricsD=merge(SpMetrics,SpeciesShort,by.x="Species",by.y="Esp")
SpMetricsD=SpMetricsD[order(SpMetricsD$Group),]

fwrite(SpMetricsD,paste0("./VigieChiro/GLMs/Summaries/",GLMPref,VarToPredict[1]
                         ,".csv"))



ModInfo=tstrsplit(ListMod,"_")
Species2=substr(ModInfo[[2]],1,nchar(ModInfo[[2]])-4)



