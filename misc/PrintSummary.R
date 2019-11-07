library(glmmTMB)

Espece="Testes"

#Modele="GLMnonselect_"
Modele="GLMnonselect_DecOT2_AT81"

FModele=paste0("./VigieChiro/GLMs/",Modele,Espece,".glm")
load(FModele)
summary(ModSp)
