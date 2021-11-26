library(data.table)
#library(readxl)
library(randomForest)

SpNuit=fread("C:/wamp64/www/SpNuit2_0_DataLP_PF_exportTot.csv")
Pheno110=fread("C:/wamp64/www/Phenosynth110.csv")
Pheno55=fread("C:/wamp64/www/Phenosynth55.csv")
Pheno22=fread("C:/wamp64/www/Phenosynth22.csv")
Pheno10=fread("C:/wamp64/www/Phenosynth10.csv")
Pheno5=fread("C:/wamp64/www/Phenosynth5.csv")
load("C:/wamp64/www/ModGites.learner")
load("C:/wamp64/www/ModRepos.learner")
OutF="C:/wamp64/www/PredictGites.csv"
#SpNuit=SpNuit[1:1000,]

RefGitesValid=SpNuit
#testSN=subset(SpNuit,SpNuit$participation==test2$participation[1])

RefGitesValid2=merge(RefGitesValid,Pheno110,by=c("participation","Nuit"
                                                ,"num_micro","espece"))
RefGitesValid2=merge(RefGitesValid2,Pheno55,by=c("participation","Nuit"
                                                 ,"num_micro","espece"))

RefGitesValid2=merge(RefGitesValid2,Pheno22,by=c("participation","Nuit"
                                                 ,"num_micro","espece"))
RefGitesValid2=merge(RefGitesValid2,Pheno10,by=c("participation","Nuit"
                                                 ,"num_micro","espece"))
RefGitesValid2=merge(RefGitesValid2,Pheno5,by=c("participation","Nuit"
                                                 ,"num_micro","espece"))



RefGitesValid2$Indice_Gite=predict(ModGites,newdata=RefGitesValid2)
RefGitesValid2$Indice_ReposNocturne=predict(ModRepos,newdata=RefGitesValid2)

fwrite(RefGitesValid2,OutF,sep=";")
