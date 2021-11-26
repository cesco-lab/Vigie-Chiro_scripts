library(data.table)
library(randomForest)

ListSpClassif=fread("Referentiel_seuils_ProbEspC3_2019-03-25_G7__D_G.csv")
SpeciesList=fread("SpeciesList.csv")

BBS=subset(SpeciesList,SpeciesList$Group %in% c("bat","bush-cricket"))

BBSmissing=subset(BBS,!(BBS$Nesp2 %in% ListSpClassif$Espece))
BBSmissing=subset(BBSmissing,BBSmissing$France=="x")

fwrite(BBSmissing,"BBSmissing.csv",sep=";")
