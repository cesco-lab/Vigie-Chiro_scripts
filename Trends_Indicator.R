library(data.table)
IndicatorList=c("Eptser","Myospp","Nyclei","Nycnoc","Pipkuh","Pippip","Pippyg")
Trends=fread("C:/Users/Yves Bas/Documents/VigieChiro/Trends/variationsAnnuellesEspece_2019-07-29.csv",dec=",")

TrendsSel=subset(Trends,Trends$espece %in% IndicatorList)
TrendsSel=subset(TrendsSel,TrendsSel$data==50)
TrendsSel2=subset(TrendsSel,TrendsSel$erreur_standard>0)

#select which threshold to be used
table(TrendsSel$espece)

TrendsSel2$weight=1/TrendsSel2$erreur_standard^2

TrendsSel2$Contrib=log(TrendsSel2$abondance_relative)*TrendsSel2$weight

AggContrib=aggregate(TrendsSel2$Contrib,by=list(TrendsSel2$year)
                    ,FUN=sum)
AggWeight=aggregate(TrendsSel2$weight,by=list(TrendsSel2$year)
                     ,FUN=sum)
IndicatorW=exp(AggContrib$x/AggWeight$x)

#IndicatorNoW=aggregate(TrendsSel$abondance_relative,by=list(TrendsSel$year),
 #                      FUN=function(a){prod(a)^(1/length(a))})

Indicator=c(1,IndicatorW)
Years=levels(as.factor(TrendsSel$year))
plot(IndicatorW)
#plot(IndicatorNoW$x)
IndicatorDF=data.frame(Indicator,Years)
reg=lm(Indicator~as.numeric(as.character(Years)))
IndicatorDF$Linear=predict(reg)

fwrite(IndicatorDF,"IndicatorDF.csv")

