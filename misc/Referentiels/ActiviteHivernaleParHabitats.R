library(data.table)
library(ggradar)

DirRef="C:/Users/yvesb/Documents/VigieChiro/Referentiels"
Pattern="2023-09-22.csv"
#Pattern2="_Hiver_"
ListHab=c("pelouses","forets de feuillus","Riviere","Urbain","Agricole")
SpSummaries=c("Pippip", "Pipkuh","Eptser","Nyclei", "Nycnoc", "Myodau", "Pipnat", "Barbar", "Pleaus"
              ,"Pippyg","Myonat","Myoema")


ListRef=list.files(DirRef,pattern=Pattern,full.names=T)
#ListRef=subset(ListRef,grepl(Pattern2,ListRef))

PropAll=vector()
Espece=vector()
Habitat=vector()
for (i in 1:length(SpSummaries)){
  print(SpSummaries[i])
  Prop=vector()
  for (j in 1:length(ListHab)){
    FRj=subset(ListRef,grepl(paste0("_",ListHab[j],"_"),ListRef))
    FREj=subset(FRj,grepl("Ete",FRj))
    REj=fread(FREj)
    matchE=match(SpSummaries[i],REj$Espece)
    ActEj=REj$MoyG[matchE]
    FREj=subset(FRj,grepl("Hiver",FRj))
    REj=fread(FREj)
    matchE=match(SpSummaries[i],REj$Espece)
    ActHj=REj$MoyG[matchE]
    Prop=c(Prop,ActHj/ActEj)
  }
  PropAll=c(PropAll,Prop)
  Espece=c(Espece,rep(SpSummaries[i],length(Prop)))
  Habitat=c(Habitat,ListHab)
}

Data=data.frame(PropAll,Espece,Habitat)
boxplot(Data$PropAll~Data$Habitat)
p <- ggplot(Data, aes(x=Habitat, y=PropAll)) + 
  geom_boxplot()
p

