library(data.table)
SpeciesIndicator=c("Pippip","Pipkuh","Nycnoc","Nyclei","Eptser","Myospp"
                   ,"Pippyg","Barbar","Pipnat","Pleaur"
                   ,"Pleaus","Hypsav")

#SpeciesIndicator=c("Tetvir","Rusnit","Leppun","Phanan","Phogri","Plaalb"
#                  ,"Testes","Phafal")
Weight=T
#SpeciesWeights=fread("NDataSp.csv")
SpeciesWeights=fread("SynthesesTendances_bat.csv")
ColW="vif_mean"
AT=fread("./output/AT_BonTron_log1.csv")

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

weighted.geomean <- function(x, w, ...)
{
  return(prod(x^w, ...)^(1/sum(w)))
}

#linear inter and extrapolation of values for grouped years
AT1=subset(AT,AT$yearRange==0)
AT2=subset(AT,AT$yearRange>0)
for (i in 1:nrow(AT2))
{
  YearSeq=c((AT2$yearMean[i]-AT2$yearRange[i]/2):(AT2$yearMean[i]+AT2$yearRange[i]/2))
  EstPrev=subset(AT,(AT$espece==AT2$espece[i])&(AT$yearMean<AT2$yearMean[i]))
  EstNext=subset(AT,(AT$espece==AT2$espece[i])&(AT$yearMean>AT2$yearMean[i]))
  ATtemp=AT2[rep(i,length(YearSeq)),]
  ATtemp$yearMean=YearSeq
  CoefL=AT2$Standsdl[i]/AT2$StandEstimates[i]
  CoefU=AT2$Standsdu[i]/AT2$StandEstimates[i]
  if(!is.na(AT2$Standsdu[i]))
  {
    if((AT2$Standsdu[i]!=Inf))
    {
      if((nrow(EstPrev)>0)&(nrow(EstNext)>0))
      {
        DataTemp=rbind(EstPrev[(nrow(EstPrev)),],AT2[i,])
        Mod1=lm(StandEstimates~yearMean,data=DataTemp)
        #summary(Mod2)
        ATtemp$StandEstimates=predict(Mod1
                                      ,newdata=ATtemp)
        
        
        DataTemp=rbind(EstNext[1,],AT2[i,])
        Mod2=lm(StandEstimates~yearMean,data=DataTemp)
        #summary(Mod2)
        ATtemp$StandEstimates[
          ((ceiling(nrow(ATtemp)/2)+1):
             nrow(ATtemp))]=predict(Mod2
                                    ,newdata=ATtemp)[
                                      ((ceiling(nrow(ATtemp)/2)+1):
                                         nrow(ATtemp))]
        
        
      }else{
        if(nrow(EstPrev)>0)
        {
          DataTemp=rbind(EstPrev[(nrow(EstPrev)),],AT2[i,])
          Mod1=lm(StandEstimates~yearMean,data=DataTemp)
          #summary(Mod2)
          ATtemp$StandEstimates=predict(Mod1,newdata=ATtemp)
          
          
        }else{
          DataTemp=rbind(EstNext[1,],AT2[i,])
          Mod2=lm(StandEstimates~yearMean,data=DataTemp)
          #summary(Mod2)
          ATtemp$StandEstimates=predict(Mod2,newdata=ATtemp)
                  }
      }
      ATtemp$Standsdl=ATtemp$StandEstimates*CoefL
      ATtemp$Standsdu=ATtemp$StandEstimates*CoefU      
      AT1=rbind(AT1,ATtemp)
    }
  }
}
table(AT1$yearMean)
table(AT1$yearMean,AT1$espece)

#rescale first year to 1
ATscale=AT1[0,]
for (j in 1:length(unique(AT1$espece)))
{
  ATj=subset(AT1,AT1$espece==unique(AT1$espece)[j])
  ATj=ATj[order(ATj$yearMean),]
  ATj$StandEstimates=ATj$StandEstimates/ATj$StandEstimates[1]
  ATj$Standsdl=ATj$Standsdl/ATj$StandEstimates[1]
  ATj$Standsdu=ATj$Standsdu/ATj$StandEstimates[1]
  
  ATscale=rbind(ATscale,ATj)
}




ATind=subset(ATscale,ATscale$espece %in% SpeciesIndicator)
table(ATind$yearMean)
fwrite(ATind,paste0("AnnualTrendsDetail_",Sys.Date(),".csv"),sep=";")
ATcast=dcast(data=ATind,yearMean~espece,fun.aggregate=mean
             ,value.var = "StandEstimates")

plot(ATcast$yearMean,as.data.frame(ATcast)[,2],type="l",ylim=c(0,2))
for (z in 2:(ncol(ATcast)-1))
{
  lines(ATcast$yearMean,as.data.frame(ATcast)[,z+1],col=z) 
}
fwrite(ATcast,paste0("AnnualTrendsSummary_",Sys.Date(),".csv"))

test=match(ColW,names(SpeciesWeights))
colnames(SpeciesWeights)[test]="weight"
ATindw=merge(ATind,SpeciesWeights,by="espece")

RawIndicator=aggregate(ATind$StandEstimates,by=list(ATind$yearMean),FUN=function(x) gm_mean(x))
plot(RawIndicator$x)
boxplot(ATind$StandEstimates~ATind$yearMean)

WeightedIndicator=vector()
LogWeightedIndicator=vector()
Wsd0=vector()
Wsd1=vector()
for (j in 1:nrow(RawIndicator))
{
  Valj=subset(ATindw,ATindw$yearMean==RawIndicator$Group.1[j])
  if(Weight)
  {
    Indw=weighted.geomean(Valj$StandEstimates,w=Valj$weight/mean(Valj$weight))
    Indw0=weighted.geomean(Valj$Standsdl,w=Valj$weight/mean(Valj$weight))
    Indw1=weighted.geomean(Valj$Standsdu,w=Valj$weight/mean(Valj$weight))
    Indlw=weighted.geomean(Valj$StandEstimates,w=log(Valj$weight+1)/mean(log(Valj$weight+1)))
  }else{
    Indw=weighted.geomean(Valj$StandEstimates,w=rep(1,nrow(Valj)))
    Indw0=weighted.geomean(Valj$Standsdl,w=rep(1,nrow(Valj)))
    Indw1=weighted.geomean(Valj$Standsdu,w=rep(1,nrow(Valj)))
    Indlw=weighted.geomean(Valj$StandEstimates,w=rep(1,nrow(Valj)))
    
  }
  WeightedIndicator=c(WeightedIndicator,Indw)
  LogWeightedIndicator=c(LogWeightedIndicator,Indlw)
  Wsd0=c(Wsd0,Indw0)
  Wsd1=c(Wsd1,Indw1)
}
colnames(RawIndicator)=c("year","RawIndicator")
plot(RawIndicator$year,WeightedIndicator,type="l",ylim=c(0,2))
lines(RawIndicator$year,LogWeightedIndicator,col=2)
lines(RawIndicator$year,RawIndicator$RawIndicator,col=3)
abline(1,0,col=2)

plot(RawIndicator$year,WeightedIndicator,type="l",ylim=c(0,2))
lines(RawIndicator$year,Wsd0,lty=2)
lines(RawIndicator$year,Wsd1,lty=2)
abline(1,0,col=2)
#plot(WeightedIndicator,RawIndicator$x)

RawIndicator$WeightedIndicator=WeightedIndicator
RawIndicator$WeightedIndicator_lse=Wsd0
RawIndicator$WeightedIndicator_use=Wsd1
fwrite(RawIndicator,paste0("IndicatorForONB_",Sys.Date(),".csv"),sep=";")

