library(data.table)

DataRP=fread("E:/RP/DataLP_RP.csv")
OutF="FmaxRP.csv"

Fmax=aggregate(DataRP$frequence,by=c(list(DataRP$participation,DataRP$Datamicro)),max)

summary(Fmax)
boxplot(Fmax$x~Fmax$Group.2)
table(Fmax$x)
fwrite(Fmax,OutF)
