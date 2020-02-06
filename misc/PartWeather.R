library(data.table)
AnomClimate=fread("./VigieChiro/Weather/Ano_Climate.csv")
AnomNCEP=fread("./VigieChiro/Weather/Ano_NCEP.csv")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
SpNuit=fread("C:/wamp64/www/SpNuit2_50_DataLP_PF_exportTot.csv")


ListPartPF=levels(as.factor(SpNuit$participation))
PartPF=subset(Particip,Particip$participation %in% ListPartPF)
SLP=merge(PartPF,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"),all=FALSE)
SpNuit_SLP=merge(SpNuit,SLP,by="participation")

SLPN=subset(SpNuit_SLP,select=c("participation","longitude","latitude"
                                ,"Nuit","protocole"))
SLPN=unique(SLPN)

SLPN$Long25=(floor(SLPN$longitude*4)/4)+0.125
SLPN$Lat25=(floor(SLPN$latitude*4)/4)+0.125
SLPN$Long25NCEP=(round(SLPN$longitude*0.4)/0.4)
SLPN$Lat25NCEP=(round(SLPN$latitude*0.4)/0.4)

SLP_C=merge(SLPN,AnomClimate,by.x=c("Long25","Lat25","Nuit")
            ,by.y=c("Long25","Lat25","V3"))

SLP_W=merge(SLP_C,AnomNCEP,by.x=c("Long25NCEP","Lat25NCEP","Nuit")
            ,by.y=c("Long25NCEP","Lat25NCEP","V3"))

SLP_W$protocoleGroup="PF"

fwrite(SLP_W,"./VigieChiro/Weather/SLP_W.csv",sep=";")


PartRP=subset(Particip,!(grepl("Fixe",Particip$site)))
SiteRP=unique(SiteLoc,by="site")
SLRP=merge(PartRP,SiteRP,by="site",all=FALSE)

SLRP$Long25=(floor(SLRP$longitude*4)/4)+0.125
SLRP$Lat25=(floor(SLRP$latitude*4)/4)+0.125
SLRP$Long25NCEP=(round(SLRP$longitude*0.4)/0.4)
SLRP$Lat25NCEP=(round(SLRP$latitude*0.4)/0.4)


SLRP$Nuit=paste(substr(SLRP$date_debut,7,10)
             ,substr(SLRP$date_debut,4,5)
             ,substr(SLRP$date_debut,1,2)
             ,sep="-"
             
)




SLRP_C=merge(SLRP,AnomClimate,by.x=c("Long25","Lat25","Nuit")
            ,by.y=c("Long25","Lat25","V3"),all.x=T)

#table(is.na(SLRP_C$TX_0_0),SLRP_C$Lat25)
test=subset(SLRP_C,is.na(SLRP_C$TX_0_0))

      
      #substr(SLRP_C$Nuit,1,4))

SLRP_W=merge(SLRP_C,AnomNCEP,by.x=c("Long25NCEP","Lat25NCEP","Nuit")
            ,by.y=c("Long25NCEP","Lat25NCEP","V3"),all.x=T)

table((SLRP_W$protocole))

SLRP_W=subset(SLRP_W,select=(colnames(SLRP_W) %in% colnames(SLP_W)))

SLRP_W$protocoleGroup="RP"

SLAll_W=rbindlist(list(SLP_W,SLRP_W),use.names=T)

table(SLAll_W$protocole,SLAll_W$protocoleGroup)
boxplot(SLAll_W$Long25NCEP~SLAll_W$protocole,ylim=c(-10,10))
boxplot(SLAll_W$Lat25NCEP~SLAll_W$protocole,ylim=c(40,55))
boxplot(SLAll_W$Long25~SLAll_W$protocole,ylim=c(-10,10))
boxplot(SLAll_W$Lat25~SLAll_W$protocole,ylim=c(40,55))
summary(is.na(SLAll_W$participation))

fwrite(SLP_W,"./VigieChiro/Weather/SLP_W.csv",sep=";")
fwrite(SLAll_W,"./VigieChiro/Weather/SLAll_W.csv",sep=";")
