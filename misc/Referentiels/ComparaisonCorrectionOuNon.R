library(data.table)

Ref1=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Ete_Total_2023-09-22.csv")
#Ref1=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels_sanscorrection/refPF_Ete_Total_2023-09-20.csv")
#Ref2=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Ete_Total_2023-09-21.csv")
SpSummaries=c("Pippip", "Pipkuh","Eptser","Nyclei", "Nycnoc", "Myodau", "Pipnat", "Barbar", "Pleaus"
              ,"Pippyg","Myonat","Myoema")

#Ref1$type="Non_Corrige"
#Ref2$type="Corrige"

#Ref12=rbind(Ref1,Ref2)

#Refsumm=subset(Ref12,Ref12$Espece %in% SpSummaries)
Refsumm=subset(Ref1,Ref1$Espece %in% SpSummaries)
Refsumm1=subset(Refsumm,select=c("Espece","Q25","Q75","Q98"))
Refsumm1$type="Corrige"
Refsumm2=subset(Refsumm,select=c("Espece","Q25nc","Q75nc","Q98nc"))
Refsumm2$type="Non_Corrige"
Refsumm=rbind(Refsumm1,Refsumm2,use.names=F)


boxplot(Refsumm$Q75~Refsumm$type)
p=ggplot(Refsumm, aes(x=Espece,y=Q75, color=type)) +
  geom_col(fill="white", position="dodge")
p

p=ggplot(Refsumm, aes(x=Espece,y=Q25, color=type)) +
  geom_col(fill="white", position="dodge")
p

p=ggplot(Refsumm, aes(x=Espece,y=Q98, color=type)) +
  geom_col(fill="white", position="dodge")
p
