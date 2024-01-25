library(data.table)
library(dplyr)

Data=fread("C:/Users/yvesb/Documents/VigieChiro/Exports/Obs_Espousouille_2022-07-26.csv")
NilssonCriteria=fread("NilssonSearch.csv")



DataN=inner_join(Data,NilssonCriteria,by=c("espece","frequence_mediane"))
DataN$DurSeq=(DataN$temps_fin-DataN$temps_debut)
summary(DataN$DurSeq)
DataN$score=DataN$proba*(DataN$temps_fin-DataN$temps_debut)

ScorePar=aggregate(DataN$score,by=list(DataN$participation),sum)
ScorePar=ScorePar[order(ScorePar$x,decreasing=T),]
head(ScorePar)

#i=1
#DataNi=subset(DataN,DataN$participation==ScorePar$Group.1[i])
#fwrite(DataNi)

test=match(DataN$participation,ScorePar$Group.1)
DataN=DataN[order(test),]

fwrite(DataN,"DataToCheckForNilsson.csv",sep=";")
