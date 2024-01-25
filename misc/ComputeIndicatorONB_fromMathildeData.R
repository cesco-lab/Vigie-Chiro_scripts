library(data.table)
library(ggplot2)

SpeciesIndicator=c("Pippip","Pipkuh","Nycnoc","Nyclei","Eptser","Myodau"
                   ,"Pipnat","Pippyg","Hypsav","Barbar","Pleaus") #bats

#SpeciesIndicator=c("Tetvir","Rusnit","Leppun","Phanan","Phogri","Plaalb"
 #                 ,"Testes","Phafal"
  #                ,"Cyrscu","Epheph","Isopyr","Roeroe","Urosp","Yerray") #bush-crickets
Weight=F
#SpeciesWeights=fread("NDataSp.csv")
SpeciesWeights=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/SynthesesTendances_bat.csv")
#SpeciesWeights=fread("SynthesesTendances_bush-cricket.csv")
ColW="vif_mean"
#AT=fread("./output/AT_MAJ200515Total.csv")
AT=fread("C:/Users/yvesb/Downloads/dataYearlyTrend.csv")
GroupedYears=F
YearRange=c(2010,2021)


AT=subset(AT,AT$year>=YearRange[1])
AT=subset(AT,AT$year<=YearRange[2])


AT$species=tstrsplit(AT$species,split="_")[[2]]

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

weighted.geomean <- function(x, w, ...)
{
  return(prod(x^w, ...)^(1/sum(w)))
}


if(GroupedYears){
#linear inter and extrapolation of values for grouped years
AT1=subset(AT,AT$yearRange==0)
AT2=subset(AT,AT$yearRange>0)
for (i in 1:nrow(AT2))
{
  YearSeq=c((AT2$year[i]-AT2$yearRange[i]/2):(AT2$year[i]+AT2$yearRange[i]/2))
  EstPrev=subset(AT,(AT$species==AT2$species[i])&(AT$year<AT2$year[i]))
  EstNext=subset(AT,(AT$species==AT2$species[i])&(AT$year>AT2$year[i]))
  ATtemp=AT2[rep(i,length(YearSeq)),]
  ATtemp$year=YearSeq
  CoefL=AT2$invLogitEstimatesInf[i]/AT2$invLogitEstimates[i]
  CoefU=AT2$invLogitEstimatesSup[i]/AT2$invLogitEstimates[i]
  if(!is.na(AT2$invLogitEstimatesSup[i]))
  {
    if((AT2$invLogitEstimatesSup[i]!=Inf))
    {
      if((nrow(EstPrev)>0)&(nrow(EstNext)>0))
      {
        DataTemp=rbind(EstPrev[(nrow(EstPrev)),],AT2[i,])
        Mod1=lm(invLogitEstimates~year,data=DataTemp)
        #summary(Mod2)
        ATtemp$invLogitEstimates=predict(Mod1
                                      ,newdata=ATtemp)
        
        
        DataTemp=rbind(EstNext[1,],AT2[i,])
        Mod2=lm(invLogitEstimates~year,data=DataTemp)
        #summary(Mod2)
        ATtemp$invLogitEstimates[
          ((ceiling(nrow(ATtemp)/2)+1):
             nrow(ATtemp))]=predict(Mod2
                                    ,newdata=ATtemp)[
                                      ((ceiling(nrow(ATtemp)/2)+1):
                                         nrow(ATtemp))]
        
        
      }else{
        if(nrow(EstPrev)>0)
        {
          DataTemp=rbind(EstPrev[(nrow(EstPrev)),],AT2[i,])
          Mod1=lm(invLogitEstimates~year,data=DataTemp)
          #summary(Mod2)
          ATtemp$invLogitEstimates=predict(Mod1,newdata=ATtemp)
          
          
        }else{
          DataTemp=rbind(EstNext[1,],AT2[i,])
          Mod2=lm(invLogitEstimates~year,data=DataTemp)
          #summary(Mod2)
          ATtemp$invLogitEstimates=predict(Mod2,newdata=ATtemp)
                  }
      }
      ATtemp$invLogitEstimatesInf=ATtemp$invLogitEstimates*CoefL
      ATtemp$invLogitEstimatesSup=ATtemp$invLogitEstimates*CoefU      
      AT1=rbind(AT1,ATtemp)
    }
  }
}
table(AT1$year)
table(AT1$year,AT1$species)
}else{
  AT1=AT
}



#rescale first year to 1
ATscale=AT1[0,]
for (j in 1:length(unique(AT1$species)))
{
  ATj=subset(AT1,AT1$species==unique(AT1$species)[j])
  ATj=ATj[order(ATj$year),]
  ToScale=ATj$invLogitEstimates[1]
  ATj$invLogitEstimates=ATj$invLogitEstimates/ToScale
  ATj$invLogitEstimatesInf=ATj$invLogitEstimatesInf/ToScale
  ATj$invLogitEstimatesSup=ATj$invLogitEstimatesSup/ToScale
  
  ATscale=rbind(ATscale,ATj)
}




ATind=subset(ATscale,ATscale$species %in% SpeciesIndicator)
table(ATind$year)
fwrite(ATind,paste0("AnnualTrendsDetail_",Sys.Date(),".csv"),sep=";")
ATcast=dcast(data=ATind,year~species,fun.aggregate=mean
             ,value.var = "invLogitEstimates")

plot(ATcast$year,as.data.frame(ATcast)[,2],type="l",ylim=c(0,2))

for (z in 2:(ncol(ATcast)-1))
{
  lines(ATcast$year,as.data.frame(ATcast)[,z+1],col=z) 
}
fwrite(ATcast,paste0("AnnualTrendsSummary_",Sys.Date(),".csv"))

test=match(ColW,names(SpeciesWeights))
colnames(SpeciesWeights)[test]="weight"
ATindw=merge(ATind,SpeciesWeights,by.x="species",by.y="espece")

RawIndicator=aggregate(ATind$invLogitEstimates,by=list(ATind$year),FUN=function(x) gm_mean(x))
plot(RawIndicator$x)
boxplot(ATind$invLogitEstimates~ATind$year)

WeightedIndicator=vector()
LogWeightedIndicator=vector()
Wsd0=vector()
Wsd1=vector()
for (j in 1:nrow(RawIndicator))
{
  Valj=subset(ATindw,ATindw$year==RawIndicator$Group.1[j])
  if(Weight)
  {
    Indw=weighted.geomean(Valj$invLogitEstimates,w=Valj$weight/mean(Valj$weight))
    Indw0=weighted.geomean(Valj$invLogitEstimatesInf,w=Valj$weight/mean(Valj$weight))
    Indw1=weighted.geomean(Valj$invLogitEstimatesSup,w=Valj$weight/mean(Valj$weight))
    Indlw=weighted.geomean(Valj$invLogitEstimates,w=log(Valj$weight+1)/mean(log(Valj$weight+1)))
  }else{
    Indw=weighted.geomean(Valj$invLogitEstimates,w=rep(1,nrow(Valj)))
    Indw0=weighted.geomean(Valj$invLogitEstimatesInf,w=rep(1,nrow(Valj)))
    Indw1=weighted.geomean(Valj$invLogitEstimatesSup,w=rep(1,nrow(Valj)))
    Indlw=weighted.geomean(Valj$invLogitEstimates,w=rep(1,nrow(Valj)))
    
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


BS_Indicator=data.frame(year=RawIndicator$year)
for (g in 1:1000)
{
  Sample=unique(ATindw$species)[sample.int(length(unique(ATindw$species))
                                          ,replace=T)]
  AT_BS=subset(ATindw,ATindw$species %in% Sample)
  WeightedIndicator=vector()
for (j in 1:nrow(RawIndicator))
{
  Valj=subset(AT_BS,AT_BS$year==RawIndicator$year[j])
  if(Weight)
  {
    Indw=weighted.geomean(Valj$invLogitEstimates,w=Valj$weight/mean(Valj$weight))
    Indw0=weighted.geomean(Valj$invLogitEstimatesInf,w=Valj$weight/mean(Valj$weight))
    Indw1=weighted.geomean(Valj$invLogitEstimatesSup,w=Valj$weight/mean(Valj$weight))
    Indlw=weighted.geomean(Valj$invLogitEstimates,w=log(Valj$weight+1)/mean(log(Valj$weight+1)))
  }else{
    Indw=weighted.geomean(Valj$invLogitEstimates,w=rep(1,nrow(Valj)))
    Indw0=weighted.geomean(Valj$invLogitEstimatesInf,w=rep(1,nrow(Valj)))
    Indw1=weighted.geomean(Valj$invLogitEstimatesSup,w=rep(1,nrow(Valj)))
    Indlw=weighted.geomean(Valj$invLogitEstimates,w=rep(1,nrow(Valj)))
    
  }
  WeightedIndicator=c(WeightedIndicator,Indw)
  LogWeightedIndicator=c(LogWeightedIndicator,Indlw)
  Wsd0=c(Wsd0,Indw0)
  Wsd1=c(Wsd1,Indw1)
}
  BS_Indicator=cbind(BS_Indicator,WeightedIndicator)
  if(g%%100==1){print(paste(g,Sys.time()))}
}

LowInt=apply(BS_Indicator[,2:ncol(BS_Indicator)],MARGIN=1
             ,FUN=function(x) quantile(x,0.025))
UpInt=apply(BS_Indicator[,2:ncol(BS_Indicator)],MARGIN=1
             ,FUN=function(x) quantile(x,0.975))
MedInt=apply(BS_Indicator[,2:ncol(BS_Indicator)],MARGIN=1
             ,FUN=function(x) quantile(x,0.5))




RawIndicator$LowInt=LowInt
RawIndicator$UpInt=UpInt
plot(RawIndicator$year,MedInt,type="l",ylim=c(0,2))
lines(RawIndicator$year,RawIndicator$UpInt,lty=2)
lines(RawIndicator$year,RawIndicator$LowInt,lty=2)
abline(1,0,col=2)

gg=ggplot(RawIndicator,aes(year,MedInt))+
  geom_line()+
  geom_pointrange(aes(ymin=LowInt, ymax=pmin(UpInt)))+
  ylim(0,max(UpInt))+
  ggtitle("Indicateur ONB Chauves-souris")+
  labs(x="Annee",y="")

print(gg)

fwrite(RawIndicator,paste0("IndicatorForONB_",Sys.Date(),".csv"),sep=";")
