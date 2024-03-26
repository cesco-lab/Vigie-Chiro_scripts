library(data.table)
library(ggplot2)
library(lubridate)
library(sf)

SpNuit=fread("C:/Users/ybas/Downloads/SpNuit2Valid_50_PG.csv")
SpeciesList=fread("C:/Users/ybas/Documents/SpeciesList.csv")
DataRP=fread("C:/Users/ybas/Downloads/DataRP_SpTron_woS_0.csv")
Particip=fread("p_export.csv")
SiteLoc=fread("C:/Users/ybas/Downloads/sites_localites.txt")
Sites=fread("C:/Users/ybas/Downloads/sites.txt")
ListVar=c("participation","nb_contacts")
ArchiveW=fread("C:/Users/ybas/Documents/www/archivees_wav.txt",h=F) #pas à jour !!
ArchiveT=fread("C:/Users/ybas/Documents/www/archivees_ta.txt",h=F) #pas à jour !!
YearLimits=c(2006:2024)
Regions=st_read("C:/Users/ybas/Documents/SIG/regions-20180101.shp"
                )
SitesRepetes=fread("C:/Users/ybas/Downloads/CalculsTendance/data/processed/Sites Repetes/toupload/data03_SR_TP_idManual_2024-02-29_103530.csv"
                   ,encoding = "Latin-1")

BatList=subset(SpeciesList,SpeciesList$Group=="bat")
BatNuit=subset(SpNuit,SpNuit$espece %in% BatList$Esp)
sum(BatNuit$nb_contacts)
BatRP=subset(DataRP,DataRP$espece %in% BatList$Esp)
sum(BatRP$nb_contacts)
BNs=subset(BatNuit,select=ListVar)
BRs=subset(BatRP,select=ListVar)
BatTot=rbind(BNs,BRs)
BatTot2=merge(BatTot,Particip,by="participation")
BatTot2$year=substr(BatTot2$date_debut,1,4)
BatTot2=subset(BatTot2,BatTot2$year %in% YearLimits)
table(BatTot2$year)
SumBat=aggregate(BatTot2$nb_contacts,by=list(BatTot2$year),FUN=sum)
# barplot(SumBat$x/1e6,names.arg=SumBat$Group.1,las=2
#         ,main="Bat passes recorded in Vigie-Chiro (in millions)")
ggplot(SumBat, aes(x = Group.1, y = x/1e6)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(title = "Bat passes recorded in Vigie-Chiro (in millions)", x = "", y = "Bat Passes (Millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
sum(SumBat$x)

#bush-crickets
BatList=subset(SpeciesList,SpeciesList$Group=="bush-cricket")
BatNuit=subset(SpNuit,SpNuit$espece %in% BatList$Esp)
sum(BatNuit$nb_contacts)
BatRP=subset(DataRP,DataRP$espece %in% BatList$Esp)
sum(BatRP$nb_contacts)
BNs=subset(BatNuit,select=ListVar)
BRs=subset(BatRP,select=ListVar)
BatTot=rbind(BNs,BRs)
BatTot2=merge(BatTot,Particip,by="participation")
BatTot2$year=substr(BatTot2$date_debut,1,4)
BatTot2=subset(BatTot2,BatTot2$year %in% YearLimits)
table(BatTot2$year)
SumBat=aggregate(BatTot2$nb_contacts,by=list(BatTot2$year),FUN=sum)
barplot(SumBat$x/1e6,names.arg=SumBat$Group.1,las=2
        ,main="Bush-cricket 5-sec files in Vigie-Chiro (in millions/year)")
ggplot(SumBat, aes(x = Group.1, y = x/1e6)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(title = "Bush-cricket 5-sec files recorded in Vigie-Chiro (in millions)", x = "", y = "Bat Passes (Millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
sum(SumBat$x)






#non-noise
BiodivList=subset(SpeciesList,SpeciesList$Nesp2!="noise")
SpNuit=subset(SpNuit,SpNuit$espece %in% BiodivList$Esp)
sum(SpNuit$nb_contacts)
DataRP=subset(DataRP,DataRP$espece %in% BiodivList$Esp)

ANs=subset(SpNuit,select=ListVar)
ARs=subset(DataRP,select=ListVar)
BiodivTot=rbind(ANs,ARs)
BatTot2=merge(BiodivTot,Particip,by="participation")
BatTot2$year=substr(BatTot2$date_debut,1,4)
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
table(SiteLoc$protocole
)



# SL_sf <- st_as_sf(SiteLoc, coords = c("longitude", "latitude"))
# SL_sf <- st_set_crs(SL_sf, 4326)
# SL_sf$protocole <- factor(SL_sf$protocole)
# table(SL_sf$protocole)
SiteLoc$protocole[SiteLoc$protocole=="CARRE"]="PEDESTRE"

color_palette <- colorRampPalette(c("green", "darkgreen"))

# Select a specific shade from the palette (e.g., the 5th shade)
intermediate_color <- color_palette(10)[5]

ggplot() +
  geom_sf(data = Regions, fill = "lightblue") +
  geom_point(data = SiteLoc[SiteLoc$protocole == "POINT_FIXE", ], aes(x = longitude, y = latitude, color = protocole), size = 2) +
  geom_point(data = SiteLoc[SiteLoc$protocole %in% c("PEDESTRE", "ROUTIER"), ], aes(x = longitude, y = latitude, color = protocole), size = 2) +
  scale_color_manual(values = c("ROUTIER" = "red", "PEDESTRE" = "blue", "POINT_FIXE" = intermediate_color)) + # Assuming 3 different values
  theme_minimal() +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 51))

SiteRepU=unique(SitesRepetes$site)
PartRepU=unique(SitesRepetes$participation)
IdSiteRepU=unique(subset(Particip$idsite,Particip$participation %in% PartRepU))

SLRepU=subset(SiteLoc,SiteLoc$id_site %in% IdSiteRepU)
table(SLRepU$protocole)

ggplot() +
  geom_sf(data = Regions, fill = "lightblue") +
  geom_point(data = SLRepU[SLRepU$protocole == "POINT_FIXE", ], aes(x = longitude, y = latitude, color = protocole), size = 2) +
  geom_point(data = SLRepU[SLRepU$protocole %in% c("PEDESTRE", "ROUTIER"), ], aes(x = longitude, y = latitude, color = protocole), size = 2) +
  scale_color_manual(values = c("ROUTIER" = "red", "PEDESTRE" = "blue", "POINT_FIXE" = intermediate_color)) + # Assuming 3 different values
  theme_minimal() +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 51))


NlocPF=subset(SiteLoc,grepl("Fixe",SiteLoc$site))
print(nrow(NlocPF))

#nb sites par région
SL_sf <- st_as_sf(NlocPF, coords = c("longitude", "latitude"))
SL_sf <- st_set_crs(SL_sf, 4326)
SL_Regions <- st_intersection(SL_sf, Regions)



table(SL_Regions$nom.1)
NbParticipantes=aggregate(SL_Regions$num.site
                          ,by=c(list(SL_Regions$nom.1)
                                ,list(SL_Regions$id_observateur)),length)
NbParticipantesPerRegion=aggregate(NbParticipantes$Group.2
                                   ,by=list(NbParticipantes$Group.1),length)

NlocR=subset(SiteLoc,grepl("Routier",SiteLoc$site))
print(length(unique(NlocR$site)))

NlocP=subset(SiteLoc,grepl("destre",SiteLoc$site))
print(length(unique(NlocP$site)))

#test=unique(Sites,by="site")
test=subset(Sites,Sites$site %in% Particip$site)

Particip$year=substr(Particip$date_debut,1,4)
test107=(Particip$year %in% YearLimits) 
summary(test107)  
Particip=subset(Particip,test107)
barplot(table(Particip$year),las=2)


ParticipUS=unique(Particip,by=c("site","year"))
data.frame((table(ParticipUS$year)))

Particip_wdata=subset(Particip,Particip$nb_obs>0)
ParticipU=unique(Particip_wdata,by=c("idobservateur","year"))
data.frame((table(ParticipU$year)))
print(length(unique(ParticipU$idobservateur)))

ParticipPF=subset(Particip,grepl("Fixe",Particip$site))
ParticipUSP=unique(ParticipPF,by=c("site","point"))
ParticipPF_wdata=subset(ParticipPF,ParticipPF$nb_obs>0)
ParticipUSP_wdata=unique(ParticipPF_wdata,by=c("site","point"))


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
sum(subset(test2$x,test2$x>1))/sum(test2$x) #%age de points r?p?t?s
Points_Representatifs=substr(PF_Bat$point,1,1)!="Z"
mean(Points_Representatifs)

#participations par protocoles
Particip$protocole=ifelse(grepl("Fixe",Particip$site)
                          ,"Stations",ifelse(grepl("Routier",Particip$site)
                                             ,"Car transects"
                                             ,"Walk transects"))
table(Particip$protocole)
#Particip$StartDate=ifelse(is.POSIXct(Particip$date_debut),Particip$date_debut,dmy_hm(Particip$date_debut))
Particip$StartDate=Particip$date_debut
#Particip$EndDate=ifelse(is.POSIXct(Particip$date_fin),Particip$date_fin,dmy_hm(Particip$date_fin))
Particip$EndDate=Particip$date_fin
Particip$Nnight=ceiling((Particip$EndDate-Particip$StartDate)/24/3600)
#Particip$Nnight=ceiling((Particip$EndDate-Particip$StartDate))
Particip$Nnight=as.numeric(Particip$Nnight)
Particip$Nnight[is.na(Particip$Nnight)]=1
Particip$Nnight[Particip$Nnight<1]=1
Particip$Nnight[Particip$Nnight>30]=1
Particip$Nnight[Particip$protocole=="Car transects"]=1
Particip$Nnight[Particip$protocole=="Walk transects"]=1
hist(Particip$Nnight,breaks=30)
sum(Particip$Nnight)
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
  geom_point(color="palegreen4")+
  xlab("")+
  ylab("")
p4

p4<-ggplot(data=pGPF, aes(x=Group.2, y=x,group=protocole)) +
  labs(title="Nb nuits") +
  geom_line(color="palegreen4")+
  geom_point(color="palegreen4")+
  xlab("")+
  ylab("")
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
p5<-ggplot(data=pG_spU, aes(x=x.y, y=x.x)) +
  geom_point()+
  xlab("Nombre de carrés/circuits")+
  ylab("Nombre de nuits")+
  scale_x_log10()+
  scale_y_log10()
p5

Obs1000N=subset(pG_spU,pG_spU$x.x>1000)
Obs100N=subset(pG_spU,pG_spU$x.x>100&pG_spU$x.x<=1000)
nrow(Obs1000N)/nrow(pG_spU)
nrow(Obs100N)/nrow(pG_spU)
sum(Obs1000N$x.x)/sum(pG_spU$x.x)
sum(Obs100N$x.x)/sum(pG_spU$x.x)
Obs1000N


print(length(unique(Particip$observateur)))
Urandom=pG_spU[sample(nrow(pG_spU)),]
fwrite(Urandom,"Urandom.csv",sep=";")
