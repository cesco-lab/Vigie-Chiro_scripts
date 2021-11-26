library(data.table)

SpNuit50=fread("C:/wamp64/www/SpNuit2_50_DataLP_PF_exportTot.csv")
SpNuit90=fread("C:/wamp64/www/SpNuit2_90_DataLP_PF_exportTot.csv")

Id50=paste(SpNuit50$participation,SpNuit50$Nuit,SpNuit50$espece)
Id90=paste(SpNuit90$participation,SpNuit90$Nuit,SpNuit90$espece)

Sp5090=subset(SpNuit50,Id50 %in% Id90)

fwrite(Sp5090,"C:/wamp64/www/SpNuit2_5090_DataLP_PF_exportTot.csv",sep=";")
