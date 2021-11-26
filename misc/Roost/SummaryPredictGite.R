library(data.table)

PG=fread("./www/PredictGites.csv")

PG0=subset(PG,PG$)
Summ=aggregate(PG$Indice_Gite,by=list(PG$espece),max)
Summ2=aggregate(PG$Indice_ReposNocturne,by=list(PG$espece),max)

Summ$RepNoc=Summ2$x

fwrite(Summ,"summaryPG.csv",sep=";")
