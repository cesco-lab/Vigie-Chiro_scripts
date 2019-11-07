library(data.table)
library(glmmTMB)
library(ggplot2)
GI=fread("./VigieChiro/GIS/Capture/GI_site_capture_chiro.csv")
Capture=fread("./VigieChiro/GIS/Capture/data_2019-06-06_clean_loc_sunset.csv")

GIC=merge(GI,Capture,by="INSEE")
GIC$yday=yday(GIC$DATE_POSIX)
GIC$BCI=GIC$POIDS/(GIC$AB^2)
hist(GIC$BCI)
GIC$SR=(GIC$SEXE=="FEMELLE")

GIC_elevage=subset(GIC,(GIC$yday>190)&(GIC$yday<230))
GIC_repro=subset(GIC,(GIC$yday>140)&(GIC$yday<230))
GIC_rut=subset(GIC,(GIC$yday>=230))

GIC_repro_adulte=subset(GIC_repro,GIC_repro$AGE!="JUVENILE")
GIC_rut_adulte=subset(GIC_rut,GIC_rut$AGE!="JUVENILE")


#BCI
for (i in 1:nlevels(as.factor(GIC_repro_adulte$TAXON)))
{
  GIC_Sp=subset(GIC_repro_adulte
                ,GIC_repro_adulte$TAXON==levels(as.factor(GIC_repro_adulte$TAXON))[i])
  if(nrow(GIC_Sp)>1000)
  {
    m1=glm(GIC_Sp$BCI~GIC_Sp$SEXE*GIC_Sp$yday+GIC_Sp$SpHC1L
           +GIC_Sp$SpHC21L+GIC_Sp$SpHC3L+GIC_Sp$SpHC4L+GIC_Sp$SpHC5L
           +GIC_Sp$SpAltiL)
    print(levels(as.factor(GIC_repro_adulte$TAXON))[i])
    print(summary(m1))
  }
}
#automne
for (i in 1:nlevels(as.factor(GIC_rut_adulte$TAXON)))
{
  GIC_Sp=subset(GIC_rut_adulte
                ,GIC_rut_adulte$TAXON==levels(as.factor(GIC_rut_adulte$TAXON))[i])
  if(nrow(GIC_Sp)>1000)
  {
    m1=glm(GIC_Sp$BCI~GIC_Sp$SEXE*GIC_Sp$yday+GIC_Sp$SpHC1L
           +GIC_Sp$SpHC3L+GIC_Sp$SpHC4L+GIC_Sp$SpHC5L
           +GIC_Sp$SpAltiL)
    print(levels(as.factor(GIC_rut$TAXON))[i])
    print(summary(m1))
  }
}


#Sex-ratio
for (i in 1:nlevels(as.factor(GIC_repro_adulte$TAXON)))
{
  GIC_Sp=subset(GIC_repro_adulte
                ,GIC_repro_adulte$TAXON==levels(as.factor(GIC_repro_adulte$TAXON))[i])
  if(nrow(GIC_Sp)>1000)
  {
    m1=glmmTMB(GIC_Sp$SR~GIC_Sp$SpHC1L
           +GIC_Sp$SpHC21L+GIC_Sp$SpHC3L+GIC_Sp$SpHC4L+GIC_Sp$SpHC5L
           +GIC_Sp$SpAltiL,family="binomial")
    print(levels(as.factor(GIC_repro_adulte$TAXON))[i])
    print(summary(m1))
  }
}

for (i in 1:nlevels(as.factor(GIC_rut_adulte$TAXON)))
{
  GIC_Sp=subset(GIC_rut_adulte
                ,GIC_rut_adulte$TAXON==levels(as.factor(GIC_rut_adulte$TAXON))[i])
  if(nrow(GIC_Sp)>1000)
  {
    m1=glmmTMB(GIC_Sp$SR~GIC_Sp$SpHC1L
               +GIC_Sp$SpHC21L+GIC_Sp$SpHC3L+GIC_Sp$SpHC4L+GIC_Sp$SpHC5L
               +GIC_Sp$SpAltiL,family="binomial")
    print(levels(as.factor(GIC_rut_adulte$TAXON))[i])
    print(summary(m1))
  }
}

#time
for (i in 1:nlevels(as.factor(GIC_repro_adulte$TAXON)))
{
  GIC_Sp=subset(GIC_repro_adulte
                ,GIC_repro_adulte$TAXON==levels(as.factor(GIC_repro_adulte$TAXON))[i])
  GIC_Sp=subset(GIC_Sp,!is.na(GIC_Sp$SEXE))
  if(nrow(GIC_Sp)>10)
  {
    Folder="./VigieChiro/GIS/Capture/PlotHeures"
    dir.create(Folder)
    png(paste0(Folder,"/",levels(as.factor(GIC_repro_adulte$TAXON))[i],"_repro.png")
        ,width = 480, height = 300)
    print(
    ggplot(GIC_Sp, aes(x=diff_sunset_heure,color=SEXE)) +
      geom_histogram(binwidth=0.2,aes(y=..density..),color="black", fill="white") +
      xlim(0,5)+
      geom_density(alpha=.2, fill="#FF6666") +
      labs(title=paste0(levels(as.factor(GIC_repro_adulte$TAXON))[i]
                        ," (période de reproduction)"),
            x ="Temps après le coucher du soleil (en h)", y = "FREQUENCE")
    )
    dev.off()
    print(levels(as.factor(GIC_repro_adulte$TAXON))[i])
    #print(hist(GIC_Sp$diff_sunset_heure
     #    ,main=paste0(levels(as.factor(GIC_repro_adulte$TAXON))[i])
      #   ,xlim=c(0,6),breaks=50))
    
      }
}


for (i in 1:nlevels(as.factor(GIC_rut_adulte$TAXON)))
{
  GIC_Sp=subset(GIC_rut_adulte
                ,GIC_rut_adulte$TAXON==levels(as.factor(GIC_rut_adulte$TAXON))[i])
  GIC_Sp=subset(GIC_Sp,!is.na(GIC_Sp$SEXE))
  if(nrow(GIC_Sp)>10)
  {
    Folder="./VigieChiro/GIS/Capture/PlotHeures"
    dir.create(Folder)
    png(paste0(Folder,"/",levels(as.factor(GIC_rut_adulte$TAXON))[i],"_rut.png")
        ,width = 480, height = 300)
    print(
      ggplot(GIC_Sp, aes(x=diff_sunset_heure,color=SEXE)) +
        geom_histogram(binwidth=0.2,aes(y=..density..),color="black", fill="white") +
        xlim(0,5)+
        geom_density(alpha=.2, fill="#FF6666") +
        labs(title=paste0(levels(as.factor(GIC_rut_adulte$TAXON))[i]
                          ," (période de rut)"),
             x ="Temps après le coucher du soleil (en h)", y = "FREQUENCE")
    )
    dev.off()
    print(levels(as.factor(GIC_rut_adulte$TAXON))[i])
    #print(hist(GIC_Sp$diff_sunset_heure
    #    ,main=paste0(levels(as.factor(GIC_rut_adulte$TAXON))[i])
    #   ,xlim=c(0,6),breaks=50))
    
  }
}
