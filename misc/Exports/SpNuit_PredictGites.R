library(data.table)

SpNuit=fread("C:/wamp64/www/SpNuit2Valid_DI_0_DataLP_PF_exportTot.csv")
Gites=fread("C:/wamp64/www/PredictGites.csv")
SpeciesList=fread("SpeciesList.csv")
OutF="C:/wamp64/www/SpNuit2Valid_50_PG.csv"

names(Gites)
test=subset(Gites,Gites$participation=="55f7d4bc6c2271000d32763c"
)


test=match(paste(SpNuit$participation,SpNuit$espece,SpNuit$Nuit
                 ,SpNuit$num_micro)
           ,paste(Gites$participation,Gites$espece,Gites$Nuit
                  ,Gites$num_micro))

SpNuit$indice_gite=Gites$Indice_Gite[test]
SpNuit$indice_reposnocturne=Gites$Indice_ReposNocturne[test]


SpNuit=subset(SpNuit,SpNuit$espece!="")
testS=match(SpNuit$espece,SpeciesList$Esp)
#table(subset(SpNuit$espece,is.na(testS)))
SpNuit$groupe=SpeciesList$GroupFR[testS]

#BatNuit=subset(SpNuit,SpNuit$groupe=="Chauve-souris")
#boxplot(BatNuit$indice_gite~BatNuit$espece,las=2,cex=0.7)
fwrite(SpNuit,OutF,sep=";")
       