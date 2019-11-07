library(data.table)
EM=fread("./mnhn/CHIRO_DIRECT_2014.csv")
FOut="DMsample.csv"

EMsample=EM[0,]
for (i in 1:nlevels(as.factor(EM$ESPECE)))
{
  EMsp=subset(EM,EM$ESPECE==levels(as.factor(EM$ESPECE))[i])
  EMsps=EMsp[sample.int(nrow(EMsp),12,replace=T),]
  EMsample=rbind(EMsample,EMsps)
}
EMsample=unique(EMsample)
table(EMsample$ESPECE)
fwrite(EMsample,FOut,sep=";")
