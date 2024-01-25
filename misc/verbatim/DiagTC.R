library(data.table)

TCsample=fread("C:/Users/yvesb/Downloads/bq-results-20220914-142754-1663165813002.csv")


TCsample$Month=substr(TCsample$xNightStart,6,7)

TCuf=unique(TCsample,by="Group_1")

table(TCsample$Month)
barplot(table(TCuf$Month))


TCsample$DurSeq=TCsample$Tend-TCsample$Tstart
TCsample$DurSeq2=round(TCsample$DurSeq)

#heure
TCsample$Hour=substr(TCsample$xDateRecord,12,13)
barplot(table(TCsample$Hour))
#amplitude


Pip=subset(TCsample,TCsample$SpMaxF2=="Pippip")


Q90=aggregate(Pip$DurSeq,by=list(Pip$xNightStart),function(x) quantile(x,0.9))
barplot(Q90$x)
testMic=(Q90$x>4.3)
barplot(testMic,names.arg=Q90$Group.1,las=2)

table(Pip$Month)
table(Pip$DurSeq2,Pip$Month)
table(Pip$xNightStart)

boxplot(Pip$DurSeq~Pip$Month)
boxplot(Pip$SuccessProb~Pip$Month)
boxplot(Pip$SuccessProb~Pip$xNightStart,las=2)
#test=boxplot(Pip$SuccessProb~Pip$xNightStart)
boxplot(Pip$SuccessProb~Pip$DurSeq2,las=2)


boxplot(Pip$NbCris~Pip$DurSeq2,las=2,ylim=c(0,100))
boxplot(Pip$NbCris~Pip$DurSeq2,las=2,ylim=c(0,100))

barplot(table(Pip$Hour))
barplot(table(Pip$Month,Pip$Hour))


Pip=subset(TCsample,TCsample$SpMaxF2=="Eptser")


table(Pip$DurSeq2,Pip$Month)
table(Pip$xNightStart)

boxplot(Pip$SuccessProb~Pip$Month)
boxplot(Pip$SuccessProb~Pip$xNightStart,las=2)
#test=boxplot(Pip$SuccessProb~Pip$xNightStart)
boxplot(Pip$SuccessProb~Pip$DurSeq2,las=2)
boxplot(Pip$SuccessProb~Pip$Month)
boxplot(Pip$SuccessProb~Pip$xNightStart,las=2)
#test=boxplot(Pip$SuccessProb~Pip$xNightStart)
boxplot(Pip$SuccessProb~Pip$DurSeq2,las=2)


boxplot(Pip$NbCris~Pip$DurSeq2,las=2,ylim=c(0,100))
boxplot(Pip$NbCris~Pip$DurSeq2,las=2,ylim=c(0,100))



Pip=subset(TCsample,TCsample$SpMaxF2=="Nycnoc")


table(Pip$DurSeq2,Pip$Month)
table(Pip$xNightStart)

boxplot(Pip$SuccessProb~Pip$Month)
boxplot(Pip$SuccessProb~Pip$xNightStart,las=2)
#test=boxplot(Pip$SuccessProb~Pip$xNightStart)
boxplot(Pip$SuccessProb~Pip$DurSeq2,las=2)
boxplot(Pip$SuccessProb~Pip$Month)
boxplot(Pip$SuccessProb~Pip$xNightStart,las=2)
#test=boxplot(Pip$SuccessProb~Pip$xNightStart)
boxplot(Pip$SuccessProb~Pip$DurSeq2,las=2)
boxplot(Pip$SuccessProb~Pip$Month)
boxplot(Pip$SuccessProb~Pip$xNightStart,las=2)
#test=boxplot(Pip$SuccessProb~Pip$xNightStart)
boxplot(Pip$SuccessProb~Pip$DurSeq2,las=2)


boxplot(Pip$NbCris~Pip$DurSeq2,las=2,ylim=c(0,100))
boxplot(Pip$NbCris~Pip$DurSeq2,las=2,ylim=c(0,100))


Pip=subset(TCsample,TCsample$SpMaxF2=="Pipnat")


table(Pip$DurSeq2,Pip$Month)
table(Pip$xNightStart)

boxplot(Pip$SuccessProb~Pip$Month)
boxplot(Pip$SuccessProb~Pip$xNightStart,las=2)
#test=boxplot(Pip$SuccessProb~Pip$xNightStart)
boxplot(Pip$SuccessProb~Pip$DurSeq2,las=2)


