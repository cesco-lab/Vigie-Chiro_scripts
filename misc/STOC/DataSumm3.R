library(data.table)

DataSumm=fread("DataSummaryOiseaux.csv")
OrderLR=c("RE","CR","EN","VU","DD","NT","LC")

RankC1=frankv(DataSumm$Occurrence,ties.method="average")
plot(RankC1)
RankC2=frankv(DataSumm$AbondanceM_SiPresence,ties.method="average",na.last=T)
plot(RankC2)

temp=DataSumm$SpecialisationForet
temp[temp<0]=0
RankI1=frankv(temp,ties.method="average")
plot(RankI1)
temp=DataSumm$SpecialisationAquatique
temp[temp<0]=0
RankI2=frankv(temp,ties.method="average")
plot(RankI2)
temp=DataSumm$SpecialisationUrbain
temp[temp<0]=0
RankI3=frankv(temp,ties.method="average")
plot(RankI3)
temp=DataSumm$SpecialisationPrairies
temp[temp<0]=0
RankI4=frankv(temp,ties.method="average")
plot(RankI4)

RankI5=frankv(DataSumm$Tendance_France_2001_2019,order=-1,ties.method="average",na.last=F)
plot(RankI5)
RankI6=frankv(DataSumm$Tendance_Europe_1980_2016,order=-1,ties.method="average",na.last=F)
plot(RankI6)
RankI7=frankv(DataSumm$Tendance_Europe_2007_2016,order=-1,ties.method="average",na.last=F)
plot(RankI7)

test=match(DataSumm$LR_Monde,OrderLR)
RankI8=frankv(test,ties.method="average",order=-1,na.last=F)
plot(RankI8)
test=match(DataSumm$LR_Europe,OrderLR)
RankI9=frankv(test,ties.method="average",order=-1,na.last=F)
plot(RankI9)
test=match(DataSumm$LR_France,OrderLR)
RankI10=frankv(test,ties.method="average",order=-1,na.last=F)
plot(RankI10)

RankI11=frankv(DataSumm$Annexe1_Directive_Oiseaux,ties.method="average",na.last=F)
plot(RankI11)

RankP1=frankv(DataSumm$PerfCall,ties.method="average",na.last=F)
plot(RankP1)
RankP2=frankv(DataSumm$PerfSong,ties.method="average",na.last=F)
plot(RankP2)

ScoreTot=(RankC1+RankC2)*
  (RankI1+RankI2+RankI3+RankI4+RankI5+RankI6+RankI7+
                            RankI8+RankI9+RankI10+RankI11)*
  (RankP1+RankP2)
ScoreRank=frankv(ScoreTot,order=-1)

DataSumm$Priorisation=ScoreRank
#table(DataSumm$LR_France)
#summary(DataSumm[,16:18])
fwrite(DataSumm,"DataSummaryOiseaux3.csv",sep=";")
