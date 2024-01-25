library(randomForest)

load("C:/Users/yvesb/Downloads/ModRFActLog_BarbarVC0V.learner")
#SpatialData=fread("C:/Users/yvesb/Downloads/GI_FR_sites_localites_PF.csv")
SpatialData=fread("C:/Users/yvesb/Downloads/Data_Barbar.csv")

ModRF$importance

varImpPlot(ModRF)

partialPlot(ModRF,SpatialData,"SpHC23L")
