library(data.table)
Besoins=as.data.frame(fread("./mnhn/besoins formation Vigie-Chiro_2019.csv",h=T))
LieuChoisi="Aude"
table(Besoins$Lieu)

#summary(Besoins)

for (i in 4:ncol(Besoins))
{
  table(Besoins[,i])
  Besoins[,i]=ifelse(Besoins[,i]=="Yes",1,ifelse(Besoins[,i]=="No",0,0.5))
  
}

Besoins=subset(Besoins,Besoins$Lieu==LieuChoisi)

BesoinNum=Besoins[,4:(ncol(Besoins)-1)]
Grouping=kmeans(BesoinNum,3)
head(names(BesoinNum)[order(Grouping$centers[1,],decreasing=T)],10)
head(names(BesoinNum)[order(Grouping$centers[2,],decreasing=T)],10)
head(names(BesoinNum)[order(Grouping$centers[3,],decreasing=T)],10)
barplot(Grouping$centers,las=2,cex.names=0.5)

Besoins$Groupe=Grouping$cluster
fwrite(Besoins,paste0("Besoins_",LieuChoisi,".csv"))
fwrite(as.data.frame(Grouping$centers)
       ,paste0("ScoresParThematiques_",LieuChoisi,".csv"))


       subset(Besoins$Participant,Besoins$Groupe==1)
       subset(Besoins$Participant,Besoins$Groupe==2)
       
       subset(Besoins$Participant,Besoins$Groupe==3)
