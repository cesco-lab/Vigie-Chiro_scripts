library(data.table)
library(glmmTMB)
library(ggeffects)
library(ggplot2)
library(moments)
library(Hmisc)
library(beepr)
VarToPredict=c("year")
VarNotToPredict=NA #list of variables in interaction with VarToPredict - TO BE SUPPRESSED in a better version...
#ToPredict=c("DecOT [((-10:10)/10)]","AT81 [-1,0,1]")
FixedLevels=c(2006:2018) # x-axis position to be predicted
GLMPref="GLM_tendancesfacteur_flexibledirect" #prefix of glm files to be used
FFBT="forBackTransform_GLM_tendancesfacteur_flexibledirectNycnoc_Seuil50.csv"



DirVTP=""
for (i in 1:length(VarToPredict))
{
  DirVTP=paste(DirVTP,VarToPredict[i],sep="_")
} 



forBackTransform=fread(paste0("./VigieChiro/GLMs/forBackTransform/",FFBT))
ListMod=list.files("./VigieChiro/GLMs/",pattern=GLMPref,full.names=T)
ListMod=subset(ListMod,grepl(".glm",ListMod))
VarMatch=match(VarToPredict,forBackTransform$VarList)


if(is.na(FixedLevels[1]))
{
  LevelsToPredict=c((-30:30)/10)
  Continuous=T
}else{
  rowtest=match(VarToPredict[1],forBackTransform$VarList)
  if(is.na(forBackTransform$Mean[rowtest]))
  {
    LevelsToPredict=FixedLevels
    Continuous=F
  }else{
    LevelsToPredict=(FixedLevels-forBackTransform$Mean[rowtest])/
      forBackTransform$Sdev[rowtest]
    Continuous=T
  }
}


ToPredict=VarToPredict
AddPredict=as.character(LevelsToPredict[1])
for (z in 2:length(LevelsToPredict))
{
  AddPredict=paste(AddPredict,LevelsToPredict[z],sep=",")
  
}
ToPredict[1]=paste0(VarToPredict[1]," [",AddPredict,"]")
if(length(ToPredict)==2){ToPredict[2]=paste(ToPredict[2],"[1,0,-1]")}

#Species=vector()
for (i in 1:length(ListMod))
{
  print(ListMod[i])
  load(ListMod[i])
  Terms=terms(ModSp)
  #TermLabels=attr(Terms,"term.labels")
  TermLabels=row.names(summary(ModSp)$coefficients$cond)
  
  TermSelect=vector()
  for(j in 1:length(VarToPredict))
  {
    TermSelect=c(TermSelect,subset(TermLabels,grepl(VarToPredict[j],TermLabels)))
  }
  if(!is.na(VarNotToPredict))
  {
    for(j in 1:length(VarNotToPredict))
    {
      TermSelect=subset(TermSelect,!grepl(VarNotToPredict[j],TermSelect))
    }
  }
  TermTarget=(!is.na(match(TermLabels,TermSelect)))
  PVal=coef(summary(ModSp))$cond[,4]
  PVal_woInt=PVal[2:length(PVal)]
  TestVar=subset(PVal_woInt,TermTarget)
  if(sum(is.na(TestVar))<length(TestVar))
  {
    #if(min(subset(TestVar,!is.na(TestVar)))<0.05)
    #{
    ModInfo=tstrsplit(ListMod[i],"_")
    #Species=c(Species,substr(ModInfo[[length(ModInfo)]],nchar(ModInfo[[length(ModInfo)]])-9,nchar(ModInfo[[length(ModInfo)]])-4))
    #Create predict table
    print(Sys.time())
    pr1.0 <- ggpredict(ModSp, c(terms = ToPredict),pretty = FALSE)
    Sys.time()
    pr1=pr1.0
    # Backtransform before scaling (utiliser la table crée lors de l'utilisation de la fonction scale)
    if (Continuous)
    {
      pr1$x=pr1$x*forBackTransform$Sdev[VarMatch[1]]+forBackTransform$Mean[VarMatch[1]]
      Xmin=quantile(pr1$x,0.05)
      Xmax=quantile(pr1$x,0.95)
    }else{
      Xmin=min(FixedLevels)
      Xmax=max(FixedLevels)
      
    }
    pr1$predicted=subset(pr1$predicted,!is.na(pr1$x))
    pr1$group=subset(pr1$group,!is.na(pr1$x))
    pr1$x=subset(pr1$x,!is.na(pr1$x))
    #pr1$invgroup=as.factor(-(as.numeric(as.character(pr1$group))))
    
    #Ymin=quantile(pr1$predicted,0.04)*0.5
    Ymin=0
    Ymax=quantile(pr1$predicted,0.99)*1.5
    pr1$conf.high=pmin(pr1$conf.high,Ymax)
    
    RawPredict=log(pr1$predicted)
    
    summary(pr1$predicted)
    
    pr1$anom=as.numeric(as.character(pr1$group))
    names(pr1)[ncol(pr1)]=VarToPredict[2]
    pr1$predicted=pmin(pr1$predicted,10000)    
    
    DirNameTemp=paste0(paste0(dirname(ListMod[i]),"/Plots/",DirVTP))
    dir.create(DirNameTemp)
    GraphNameTemp=paste0(DirNameTemp,"/",gsub(".glm","",basename(ListMod[i])),".png")
    
    
    # Plot
    #GradColor=scale_color_gradient2(low="blue",mid="green",high="red")
    if(nlevels(pr1$group)==3)
    {
      if(!is.na(match("facet",names(pr1)))){
        for (j in 1:nlevels(as.factor(pr1$facet)))
        {
          DirNameTemp=paste0(paste0(dirname(ListMod[i]),"/Plots/",DirVTP))
          dir.create(DirNameTemp)
          GraphNameTemp=paste0(DirNameTemp,"/",j,"_",gsub(".glm","",basename(ListMod[i])),".png")
          png(filename=GraphNameTemp, res=100)
          
          prtemp=subset(pr1,pr1$facet==levels(as.factor(pr1$facet))[j])
          print(ggplot(prtemp, aes(x, predicted,fill=group)) +
                  #scale_color_gradient2(low="blue",mid="green",high="red") +
                  #scale_colour_discrete(name = "anom")+
                  scale_color_manual(values=c("blue","green","red"))+
                  geom_line(aes(color = group),size=1)  +
                  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
                  geom_ribbon(data=prtemp[prtemp[,ncol(prtemp)]==1,]
                              ,aes(ymin = conf.low, ymax = conf.high), alpha = .1
                              ,fill="red")+
                  geom_ribbon(data=prtemp[prtemp[,ncol(prtemp)]==0,]
                              ,aes(ymin = conf.low, ymax = conf.high), alpha = .1
                              ,fill="green")+
                  geom_ribbon(data=prtemp[prtemp[,ncol(prtemp)]==-1,]
                              ,aes(ymin = conf.low, ymax = conf.high), alpha = .1
                              ,fill="blue")+
                  xlab(VarToPredict) + 
                  ylab("Acoustic Activity") +
                  scale_x_continuous(limits = c(Xmin, Xmax)) +
                  scale_y_continuous(limits = c(Ymin, Ymax)) +
                  theme_bw(base_size = 10)+
                  
                  ggtitle(gsub(".png","",basename(GraphNameTemp))) 
                #theme(plot.title = element_text(size = 8))+
                #scale_fill_discrete(values=c("blue","green","red"),guide=FALSE)+
          )
          dev.off()
        }
      }else{
        DirNameTemp=paste0(paste0(dirname(ListMod[i]),"/Plots/",DirVTP))
        dir.create(DirNameTemp)
        GraphNameTemp=paste0(DirNameTemp,"/",gsub(".glm","",basename(ListMod[i])),".png")
        png(filename=GraphNameTemp, res=100)
        
        prtemp=pr1
        print(ggplot(prtemp, aes(x, predicted,fill=group)) +
                #scale_color_gradient2(low="blue",mid="green",high="red") +
                #scale_colour_discrete(name = "anom")+
                scale_color_manual(values=c("blue","green","red"))+
                geom_line(aes(color = group),size=1)  +
                #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
                geom_ribbon(data=prtemp[prtemp[,ncol(prtemp)]==1,]
                            ,aes(ymin = conf.low, ymax = conf.high), alpha = .1
                            ,fill="red")+
                geom_ribbon(data=prtemp[prtemp[,ncol(prtemp)]==0,]
                            ,aes(ymin = conf.low, ymax = conf.high), alpha = .1
                            ,fill="green")+
                geom_ribbon(data=prtemp[prtemp[,ncol(prtemp)]==-1,]
                            ,aes(ymin = conf.low, ymax = conf.high), alpha = .1
                            ,fill="blue")+
                xlab(VarToPredict) + 
                ylab("Acoustic Activity") +
                scale_x_continuous(limits = c(Xmin, Xmax)) +
                scale_y_continuous(limits = c(Ymin, Ymax)) +
                theme_bw(base_size = 10)+
                
                ggtitle(gsub(".png","",basename(GraphNameTemp))) 
              #theme(plot.title = element_text(size = 8))+
              #scale_fill_discrete(values=c("blue","green","red"),guide=FALSE)+
        )
        dev.off()
        
      }  
    }else{
      png(filename=GraphNameTemp, res=100)
      
      print(ggplot(pr1, aes(x, predicted)) +
              geom_line()  +
              #scale_color_gradient2(low="blue",mid="green",high="red") +
              geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
              xlab(VarToPredict) + 
              ylab("Acoustic Activity") +
              scale_x_continuous(limits = c(Xmin, Xmax)) +
              scale_y_continuous(limits = c(Ymin, Ymax)) +
              
              ggtitle(gsub(".png","",basename(GraphNameTemp))) + 
              theme_bw(base_size = 13)
      )
      dev.off()
      
    }
  }
}
#}

beep()
