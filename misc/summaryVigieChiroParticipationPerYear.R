library(data.table)
library(ggplot2)
library(lubridate)

SpNuit=fread("./www/SpNuit2_0_DataLP_PF_exportTot.csv")
SpeciesList=fread("SpeciesList.csv")
DataRP=fread("./vrac_md_dell2021/DataRP_SpSecteur_0.csv")
Particip=fread("./www/p_export_forLinux.csv")
SiteLoc=fread("./www/sites_localites.txt")
Sites=fread("./www/sites.txt")
ListVar=c("participation","nb_contacts")
ArchiveW=fread("./www/archivees_wav.txt",h=F)
ArchiveT=fread("./www/archivees_ta.txt",h=F)

BatList=subset(SpeciesList,SpeciesList$Group=="bat")
BatNuit=subset(SpNuit,SpNuit$espece %in% BatList$Esp)
sum(BatNuit$nb_contact)
BatRP=subset(DataRP,DataRP$espece %in% BatList$Esp)
sum(BatRP$nb_contact)
BNs=subset(BatNuit,select=ListVar)
BRs=subset(BatRP,select=ListVar)
BatTot=rbind(BNs,BRs)
BatTot2=merge(BatTot,Particip,by="participation")
BatTot2$year=substr(BatTot2$date_debut,7,10)
table(BatTot2$year)
SumBat=aggregate(BatTot2$nb_contacts,by=list(BatTot2$year),FUN=sum)
barplot(SumBat$x/1e6,names.arg=SumBat$Group.1,las=2
        ,main="Bat passes recorded in Vigie-Chiro (in millions/year)")
sum(SumBat$x)

#bush-crickets
BatList=subset(SpeciesList,SpeciesList$Group=="bush-cricket")
BatNuit=subset(SpNuit,SpNuit$espece %in% BatList$Esp)
sum(BatNuit$nb_contact)
BatRP=subset(DataRP,DataRP$espece %in% BatList$Esp)
sum(BatRP$nb_contact)
BNs=subset(BatNuit,select=ListVar)
BRs=subset(BatRP,select=ListVar)
BatTot=rbind(BNs,BRs)
BatTot2=merge(BatTot,Particip,by="participation")
BatTot2$year=substr(BatTot2$date_debut,7,10)
table(BatTot2$year)
SumBat=aggregate(BatTot2$nb_contacts,by=list(BatTot2$year),FUN=sum)
barplot(SumBat$x/1e6,names.arg=SumBat$Group.1,las=2
        ,main="Bush-cricket 5-sec files in Vigie-Chiro (in millions/year)")
sum(SumBat$x)




#non-noise
BiodivList=subset(SpeciesList,SpeciesList$Nesp2!="noise")
SpNuit=subset(SpNuit,SpNuit$espece %in% BiodivList$Esp)
sum(SpNuit$nb_contact)
DataRP=subset(DataRP,DataRP$espece %in% BiodivList$Esp)

ANs=subset(SpNuit,select=ListVar)
ARs=subset(DataRP,select=ListVar)
BiodivTot=rbind(ANs,ARs)
BatTot2=merge(BiodivTot,Particip,by="participation")
BatTot2$year=substr(BatTot2$date_debut,7,10)
table(BatTot2$year)
SumAll=aggregate(BatTot2$nb_contacts,by=list(BatTot2$year),FUN=sum)
barplot(SumAll$x/1e6,names.arg=SumAll$Group.1,las=2
        ,main="Biodiv records in Vigie-Chiro (in millions/year)")
sum(SumAll$x)

SumAll=aggregate(BatTot2$nb_contacts,by=list(BatTot2$year),FUN=length)
barplot(SumAll$x/1e3,names.arg=SumAll$Group.1,las=2
        ,main="Biodiv date-locality records in Vigie-Chiro (in thousands/year)")
sum(SumAll$x)

#Number of sites
NsRP=subset(Sites,!grepl("Fixe",Sites$site))
print(table(grepl("Routier",NsRP$site)))

NlocPF=subset(SiteLoc,grepl("Fixe",SiteLoc$site))
print(nrow(NlocPF))
#test=unique(Sites,by="site")
test=subset(Sites,Sites$site %in% Particip$site)

Particip$year=substr(Particip$date_debut,7,10)
ParticipUS=unique(Particip,by=c("site","year"))
data.frame((table(ParticipUS$year)))
ParticipU=unique(Particip,by=c("idobservateur","year"))
data.frame((table(ParticipU$year)))



#Number of points-transects
PoinTransects=subset(SiteLoc,(!grepl(" ",SiteLoc$nom)|
                                substr(SiteLoc$nom,nchar(SiteLoc$nom)
                                       ,nchar(SiteLoc$nom))=="3"))
print(nrow(PoinTransects))

#Number of points-sectors
print(nrow(SiteLoc))

#Rate of wave archiving
TxArchivW=nrow(ArchiveW)/(nrow(ArchiveW)+nrow(ArchiveT))
PW=subset(Particip,Particip$participation %in% ArchiveW$V1)
PT=subset(Particip,Particip$participation %in% ArchiveT$V1)
print(sum(PW$nb_don)/(sum(PW$nb_don)+sum(PT$nb_don)))

p<-ggplot(data=SumBat, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") +
  labs(title="Bat passes recorded in Vigie-Chiro per year"
       ,x="",y="")
p

RP_Bat=subset(BatTot2,!grepl("Fixe",BatTot2$site))
SumBat=aggregate(RP_Bat$nb_contacts,by=list(RP_Bat$year),FUN=sum)

p2<-ggplot(data=SumBat, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") +
  labs(title="Bat passes in transects"
       ,x="",y="")
p2

PF_Bat=subset(BatTot2,grepl("Fixe",BatTot2$site))
SumBat=aggregate(PF_Bat$nb_contacts,by=list(PF_Bat$year),FUN=sum)

p3<-ggplot(data=SumBat, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") +
  labs(title="Nb contacts Point Fixe"
       ,x="",y="")
p3

dim(unique(SpNuit,by=c("Nuit","participation")))
dim(unique(PF_Bat,by=c("site","point")))

test=aggregate(PF_Bat$nb_contacts,by=c(list(PF_Bat$year),list(PF_Bat$site),list(PF_Bat$point))
               ,FUN=length)
test2=aggregate(test$Group.1,by=c(list(test$Group.2),list(test$Group.3)),FUN=length)
summary(test2$x>1)
sum(subset(test2$x,test2$x>1))/sum(test2$x) #%age de points répétés
Points_Representatifs=substr(PF_Bat$point,1,1)!="Z"
mean(Points_Representatifs)

#participations par protocoles
Particip$protocole=ifelse(grepl("Fixe",Particip$site)
                          ,"Stations",ifelse(grepl("Routier",Particip$site)
                                             ,"Car transects"
                                             ,"Walk transects"))
table(Particip$protocole)
Particip$StartDate=dmy_hm(Particip$date_debut)
Particip$EndDate=dmy_hm(Particip$date_fin)

Particip$Nnight=ceiling((Particip$EndDate-Particip$StartDate)/24/3600)
Particip$Nnight=as.numeric(Particip$Nnight)
Particip$Nnight[is.na(Particip$Nnight)]=1
Particip$Nnight[Particip$Nnight<1]=1
Particip$Nnight[Particip$Nnight>30]=1
Particip$Nnight[Particip$protocole=="Car transects"]=1
Particip$Nnight[Particip$protocole=="Walk transects"]=1
hist(Particip$Nnight,breaks=30)
Particip=subset(Particip,Particip$year>2005)
Particip=subset(Particip,Particip$year<year(Sys.Date()))

aggregate(Particip$Nnight,by=list(Particip$protocole),sum)
table(Particip$Nnight)
pGnights=aggregate(Particip$Nnight,by=c(list(Particip$protocole)
                                        ,list(Particip$year)),sum)
pGnights$protocole=pGnights$Group.1
p4<-ggplot(data=pGnights, aes(x=Group.2, y=x,group=protocole)) +
  labs(title="Nb nights*sites per protocoles") +
  geom_line(aes(color=protocole))+
  geom_point(aes(color=protocole))
p4

pGRP=subset(pGnights,pGnights$Group.1!="Stations")
p4<-ggplot(data=pGRP, aes(x=Group.2, y=x,group=protocole)) +
  labs(title="Nb transects per protocoles") +
  geom_line(aes(color=protocole))+
  geom_point(aes(color=protocole))
p4
pGPF=subset(pGnights,pGnights$Group.1=="Stations")
pGPF=subset(pGPF,pGPF$Group.2>2013)

p4<-ggplot(data=pGPF, aes(x=Group.2, y=x,group=protocole)) +
  labs(title="Nb stations*nights per year") +
  geom_line(color="palegreen4")+
  geom_point(color="palegreen4")
p4
#mod=glm(pGPF$x~as.numeric(pGPF$Group.2),family="poisson")
#summary(mod)
print(paste("yearly increase:",round((mean(pGPF$x[2:nrow(pGPF)]/
                                               pGPF$x[1:(nrow(pGPF)-1)])-1)*100)
            ,"%"))


pG_pU=aggregate(Particip$Nnight,by=c(list(Particip$observateur)
                                     ),sum)
pG_psU=aggregate(Particip$Nnight,by=c(list(Particip$observateur)
                                      ,list(Particip$site)
),length)

pG_sU=aggregate(pG_psU$Group.2,by=list(pG_psU$Group.1),length)
                                     
pG_spU=merge(pG_pU,pG_sU,by="Group.1")
#pG_spU$x.y[is.na(pG_spU$x.y)]=1
plot(pG_spU$x.x,pG_spU$x.y,log="xy")

p5<-ggplot(data=pG_spU, aes(x=x.y, y=x.x)) +
  geom_point()+
  xlab("number of transects/squares")+
  ylab("number of nights sampled")+
  scale_x_log10()+
  scale_y_log10()
p5

print(length(unique(Particip$observateur)))
Urandom=pG_spU[sample(nrow(pG_spU)),]
fwrite(Urandom,"Urandom.csv",sep=";")
