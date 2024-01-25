library(data.table)
library(dplyr)
library(forcats)

RefEte=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Ete_Total_2023-09-22.csv")
RefEteM=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Ete_mediterranean_2023-09-22.csv")
RefAutomne=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Automne_Total_2023-09-22.csv")
RefAutomneM=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Automne_mediterranean_2023-09-22.csv")
RefHiver=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Hiver_Total_2023-09-22.csv")
RefHiverM=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Hiver_mediterranean_2023-09-22.csv")
RefPrintemps=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Printemps_Total_2023-09-22.csv")
RefPrintempsM=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Printemps_mediterranean_2023-09-22.csv")
SpSummaries=c("Pippip", "Pipkuh","Eptser","Nyclei", "Nycnoc", "Myodau", "Pipnat", "Barbar", "Pleaus"
              ,"Pippyg","Myonat","Myoema")
OrderSeason=c("Hiver","Printemps","Ete","Automne")

RefEte$type="Ete"
RefPrintemps$type="Printemps"
RefAutomne$type="Automne"
RefHiver$type="Hiver"

Ref4saisons=rbind(RefHiver,RefPrintemps,RefEte,RefAutomne)

Refsumm=subset(Ref4saisons,Ref4saisons$Espece %in% SpSummaries)
Match19=match(Refsumm$type,OrderSeason)
Refsumm$order=Match19

Refsumm %>%
  arrange(type) %>%
  mutate(type = as.factor(type),
         type = fct_relevel(type, "Hiver", "Printemps", "Ete", "Automne"))%>%
ggplot(aes(x=Espece,y=MoyG, color=type))+  geom_col(fill="white",position="dodge")+
  scale_colour_manual(values=c("blue","darkgreen","orange","brown"))

RefEteM$type="Ete"
RefPrintempsM$type="Printemps"
RefAutomneM$type="Automne"
RefHiverM$type="Hiver"

Ref4saisons=rbind(RefHiverM,RefPrintempsM,RefEteM,RefAutomneM)

Refsumm=subset(Ref4saisons,Ref4saisons$Espece %in% SpSummaries)
Match19=match(Refsumm$type,OrderSeason)
Refsumm$order=Match19

Refsumm %>%
  arrange(type) %>%
  mutate(type = as.factor(type),
         type = fct_relevel(type, "Hiver", "Printemps", "Ete", "Automne"))%>%
  ggplot(aes(x=Espece,y=MoyG, color=type))+  geom_col(fill="white",position="dodge")+
  scale_colour_manual(values=c("blue","darkgreen","orange","brown"))


Ref1=subset(RefEte,select=c("Espece","MoyG"))
Ref2=subset(RefHiver,select=c("Espece","MoyG"))
Match33=match(Ref1$Espece,Ref2$Espece)
Ref1$Activite_hivernale=Ref2$MoyG[Match33]

Refsumm=subset(Ref1,Ref1$Espece %in% SpSummaries)

Refsumm$ProportionHiverEte=Refsumm$Activite_hivernale/Refsumm$MoyG
ggplot(Refsumm,aes(x=Espece,y=ProportionHiverEte))+
  geom_col(position="dodge")+
  ggtitle("France entiere")
  

Ref1=subset(RefEteM,select=c("Espece","MoyG"))
Ref2=subset(RefHiverM,select=c("Espece","MoyG"))
Match33=match(Ref1$Espece,Ref2$Espece)
Ref1$Activite_hivernale=Ref2$MoyG[Match33]

Refsumm=subset(Ref1,Ref1$Espece %in% SpSummaries)

Refsumm$ProportionHiverEte=Refsumm$Activite_hivernale/Refsumm$MoyG
ggplot(Refsumm,aes(x=Espece,y=ProportionHiverEte))+
  geom_col(position="dodge")+
  ggtitle("Zone mediterraneenne")


