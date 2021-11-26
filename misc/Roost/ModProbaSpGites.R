library(data.table)
library(readxl)
library(randomForest)

#SpNuit=fread("C:/wamp64/www/SpNuit2_0_DataLP_PF_exportTot.csv")
Pheno110=fread("C:/wamp64/www/Phenosynth110.csv")
Pheno55=fread("C:/wamp64/www/Phenosynth55.csv")
Pheno22=fread("C:/wamp64/www/Phenosynth22.csv")
Pheno10=fread("C:/wamp64/www/Phenosynth10.csv")
Pheno5=fread("C:/wamp64/www/Phenosynth5.csv")
#RefGites=read_xlsx("C:/wamp64/www/SpNuit_Gites_qualif.xlsx")
RefGites=fread("C:/wamp64/www/ValidationsGite.csv")

Pheno10$V5=NULL #oooh dirty....

RefGitesValid=subset(RefGites,!is.na(RefGites$gite_diurne))
RefGitesValid$Nuit=substr(RefGitesValid$Nuit,1,10)

test=match(paste(RefGitesValid$participation,RefGitesValid$Nuit
                 ,RefGitesValid$num_micro,RefGitesValid$espece)
           ,paste(Pheno110$participation,Pheno110$Nuit,Pheno110$num_micro
                  ,Pheno110$espece))
test2=subset(RefGitesValid,is.na(test))

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


Predictors=subset(names(RefGitesValid2)
                  ,!names(RefGitesValid2) %in% c("participation","Nuit"
                                                 ,"num_micro","espece"
                                                 ,"groupe"
                                                 ,"min_decalage_coucher"
                                                 ,"gite_diurne"
                                                 ,"repos_nocturne"
                                                 ,"site","point"
                                                 ,"randorder"
                                              ))

TablePredictors=subset(RefGitesValid2,select=Predictors)
ModGites=randomForest(y=RefGitesValid2$gite_diurne,x=TablePredictors
                      ,strata=RefGitesValid2$participation)
ModRepos=randomForest(y=RefGitesValid2$repos_nocturne,x=TablePredictors
                      ,strata=RefGitesValid2$participation)


RefGitesNonValid=subset(RefGites,is.na(RefGites$gite_diurne))
RefGitesNonValid$Nuit=substr(RefGitesNonValid$Nuit,1,10)

RefGitesNonValid2=merge(RefGitesNonValid,Pheno110,by=c("participation","Nuit"
                                                 ,"num_micro","espece"))
RefGitesNonValid2=merge(RefGitesNonValid2,Pheno55,by=c("participation","Nuit"
                                                 ,"num_micro","espece"))

RefGitesNonValid2=merge(RefGitesNonValid2,Pheno22,by=c("participation","Nuit"
                                                 ,"num_micro","espece"))
RefGitesNonValid2=merge(RefGitesNonValid2,Pheno10,by=c("participation","Nuit"
                                                 ,"num_micro","espece"))
RefGitesNonValid2=merge(RefGitesNonValid2,Pheno5,by=c("participation","Nuit"
                                                ,"num_micro","espece"))
test=subset(rownames(ModGites$importance)
            ,!rownames(ModGites$importance) %in% names(RefGitesNonValid2))
test2=lapply(RefGitesNonValid2,function(x) class(x))
table(unlist(test2))
subset(names(test2),test2=="character")
test3=lapply(RefGitesNonValid2,function(x) (sum(is.na(x))))
plot(unlist(test3))

RefGitesNonValid2$gite_diurne=NULL
RefGitesNonValid2$repos_nocturne=NULL


RefGitesNonValid2$probabiliteGite=predict(ModGites,newdata=RefGitesNonValid2)
RefGitesNonValid2$probabiliteReposNocturne=predict(ModRepos,newdata=RefGitesNonValid2)

fwrite(RefGitesNonValid2,"C:/wamp64/www/RefGitesNonValid2.csv",sep=";")
save(ModGites,file="C:/wamp64/www/ModGites.learner")
save(ModRepos,file="C:/wamp64/www/ModRepos.learner")

RefGites$Nuit=substr(RefGites$Nuit,1,10)

RefGites2=merge(RefGites,Pheno110,by=c("participation","Nuit"
                                                       ,"num_micro","espece"))
RefGites2=merge(RefGites2,Pheno55,by=c("participation","Nuit"
                                                       ,"num_micro","espece"))

RefGites2=merge(RefGites2,Pheno22,by=c("participation","Nuit"
                                                       ,"num_micro","espece"))
RefGites2=merge(RefGites2,Pheno10,by=c("participation","Nuit"
                                                       ,"num_micro","espece"))
RefGites2=merge(RefGites2,Pheno5,by=c("participation","Nuit"
                                                      ,"num_micro","espece"))
test=subset(rownames(ModGites$importance)
            ,!rownames(ModGites$importance) %in% names(RefGites2))
test2=lapply(RefGites2,function(x) class(x))
table(unlist(test2))
subset(names(test2),test2=="character")
test3=lapply(RefGites2,function(x) (sum(is.na(x))))
plot(unlist(test3))

RefGites2$gite_diurne=NULL
RefGites2$repos_nocturne=NULL


RefGites2$probabiliteGite=predict(ModGites,newdata=RefGites2)
RefGites2$probabiliteReposNocturne=predict(ModRepos,newdata=RefGites2)
fwrite(RefGites2,"C:/wamp64/www/RefGites2.csv",sep=";")
