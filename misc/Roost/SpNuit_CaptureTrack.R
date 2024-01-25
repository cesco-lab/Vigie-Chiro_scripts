library(readxl)
library(data.table)
library(raster)
library(lubridate)

DiffDay=30
DateT="2022-11-01"
SpNuit=fread("C:/Users/yvesb/Documents/www/SpNuit2Valid_50_PG.csv")
Particip=fread("C:/Users/yvesb/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/p_export_forLinux.csv")
SiteLoc=fread("C:/Users/yvesb/Documents/www/sites_localites.csv")
Ref=fread("C:/Users/yvesb/Documents/VigieChiro/Referentiels/refPF_Ete_Total_2023-09-29.csv")
SpeciesList=fread("C:/Users/yvesb/Documents/SpeciesList.csv")
ListeBats=read_xlsx("C:/Users/yvesb/Documents/ListeBats.xlsx")
SiteSw=read_xlsx("C:/Users/yvesb/Documents/Sites_Swarming.xlsx")
#RefGites=fread("C:/Users/yvesb/Documents/www/RefGites2.csv")
DataGCLR=fread("C:/Users/yvesb/Documents/chiros/BDD_GCLR/20160108GCLR_XPORT_toutes_donnees.csv",encoding="UTF-8")
DataGCLRcompl=read_xlsx("C:/Users/yvesb/Documents/chiros/BDD_GCLR/complements/AllData_short.xlsx")
#Fdep=shapefile("C:/Users/yvesb/Documents/SIG/Limite_administrative/France_dep_L93.shp")
Fw84=shapefile("C:/Users/yvesb/Documents/France_wgs84.shp")

CoordO=c(43.826,3.77) # LD
#CoordO=c(42.62579592335401, 2.0980440005120045) # Espousouille
#CoordO=c(43.93493682074521, 4.3207776) #sainte-anastasie

DataCapturePerso=fread("C:/Users/yvesb/Documents/chiros/DataAllCapture.csv")

#Fw84=spTransform(Fdep,CRS("+init=epsg:4326"))

SpNuit=subset(SpNuit,SpNuit$num_micro==0)

DataCapturePerso$espece[DataCapturePerso$espece=="Myomyo"]="MyoGT"
DataCapturePerso$espece[DataCapturePerso$espece=="Myobly"]="MyoGT"
DataCapturePerso$espece[DataCapturePerso$espece=="Myocry"]="Myonat"

ListeBats$Espece[ListeBats$Espece=="Myobly"]="MyoGT"
ListeBats$Espece[ListeBats$Espece=="Myocry"]="Myonat"
Batlist=subset(SpeciesList,SpeciesList$Group=="bat")


DataGCLR$MB=ifelse(DataGCLR$gite=="Gîte de reproduction",T
                   ,ifelse(DataGCLR$stade=="Juvénile/Immature",T,F))
summary(DataGCLR$MB)


table(DataGCLRcompl$Protocole)
DataGCLRcompl$Détermination=ifelse(grepl("gîte",DataGCLRcompl$Protocole),"Vu",DataGCLRcompl$Détermination)
DataGCLRcompl$Détermination=ifelse(grepl("nichoir",DataGCLRcompl$Protocole),"Vu",DataGCLRcompl$Détermination)
table(DataGCLRcompl$Détermination)

#compilation données GCLR



DataGCLRcompl$date_debut=paste0(day(DataGCLRcompl$`Date obs.`),"/"
                                ,month(DataGCLRcompl$`Date obs.`),
                                "/",year(DataGCLRcompl$`Date obs.`))
#DataGCLRcompl$date_debut=as.Date(DataGCLRcompl$`Date obs.`,tryFormats="%d/%m/%Y")

DataGCLRcompl$nom_latin=tstrsplit(DataGCLRcompl$`Nom latin/complet`, split=" \\(")[[1]]
DataGCLRcompl$x=DataGCLRcompl$Longitude
DataGCLRcompl$y=DataGCLRcompl$Latitude
DataGCLRcompl$contact=DataGCLRcompl$Détermination



test=subset(DataGCLRcompl,grepl("gîte",DataGCLRcompl$Protocole))
DataGCLRcompl$MB=(grepl("gîte",DataGCLRcompl$Protocole)&(DataGCLRcompl$Activité=="Allaitante"))
summary(DataGCLRcompl$MB)
DataGCLRcompl$MB[is.na(DataGCLRcompl$MB)]=F




lDateT=ymd(DateT)
yDateT=yday(lDateT)

test=subset(SpNuit,SpNuit$participation=="5b59fe66b6ee94000f23939c")


BatNuit=subset(SpNuit,SpNuit$espece %in% Batlist$Esp)
BatNuit=subset(BatNuit,BatNuit$score_max>0.5)
testR=match(BatNuit$espece,Ref$Espece)
summary(testR)
table(subset(BatNuit$espece,is.na(testR)))
BatNuit$refH=Ref$Q75[testR]
testB=match(BatNuit$espece,ListeBats$Espece)
table(subset(BatNuit$espece,is.na(testB)))
BatNuit$CoefSp=ListeBats$Priorités[testB]
BatNuit$Radius=ListeBats$`Rayon d'action`[testB]
BatNuit$IndiceCap=BatNuit$nb_contacts_nd/BatNuit$refH*BatNuit$CoefSp*
  BatNuit$score_max
testP=match(BatNuit$participation,Particip$participation)
testPS=match(paste(Particip$site,Particip$point)
             ,paste(SiteLoc$site,SiteLoc$nom))
BatNuit$longitude=SiteLoc$longitude[testPS[testP]]
BatNuit$latitude=SiteLoc$latitude[testPS[testP]]
BatNuit$yday=yday(ymd(BatNuit$Nuit))
BatNuit$diffyday=abs(BatNuit$yday-yDateT)

BatNuitPeriod=subset(BatNuit,BatNuit$diffyday<=DiffDay)
table(BatNuitPeriod$espece)
ClosestDateB=aggregate(BatNuitPeriod$diffyday
                       ,by=c(list(BatNuitPeriod$longitude)
                             , list(BatNuitPeriod$latitude)), min)
testCB=match(paste(BatNuitPeriod$longitude,BatNuitPeriod$latitude
                   ,BatNuitPeriod$diffyday)
             ,paste(ClosestDateB$Group.1,ClosestDateB$Group.2,ClosestDateB$x))
BatNuitPeriodC=subset(BatNuitPeriod,!is.na(testCB))




#sort out known roosts
#from VigieChiro
#GitesVC=subset(RefGites,RefGites$probabiliteGite>0.5)
testGP=match(BatNuitPeriodC$participation,Particip$participation)
sum(is.na(testGP))
sum(is.na(testPS))

#BatNuitPeriodC$longitude=SiteLoc$longitude[testPS[testGP]]
#SpNuit$latitude=SiteLoc$latitude[testPS[testGP]]
BatNuitPeriodC$gite=SiteLoc$SpGite[testPS[testGP]]
summary(BatNuitPeriodC$gite)
RefGites=subset(BatNuitPeriodC,BatNuitPeriodC$gite==1)
RefGites$yday=yday(ymd(RefGites$Nuit))
RefGites$diffyday=abs(RefGites$yday-yDateT)
ClosestDate=aggregate(RefGites$diffyday
                      ,by=c(list(RefGites$longitude)
                            , list(RefGites$latitude)), min)
testC=match(paste(RefGites$longitude,RefGites$latitude,RefGites$diffyday)
            ,paste(ClosestDate$Group.1,ClosestDate$Group.2,ClosestDate$x))
RefGitePeriod=subset(RefGites,!is.na(testC))
fwrite(RefGitePeriod,paste0("C:/Users/yvesb/Documents/www/RefGitesPeriod",DateT,".csv"),sep=";")
summary(RefGitePeriod$diffyday)
length(unique(paste(RefGitePeriod$latitude,RefGitePeriod$longitude)))

TrueGiteP=subset(RefGitePeriod,RefGitePeriod$indice_gite>0.5)
table(TrueGiteP$espece)

#convert new data in L93
#DataGCLRcompl=subset(DataGCLRcompl,!is.na(DataGCLRcompl$y))
DataGCLRcompl_ref=DataGCLRcompl
coordinates(DataGCLRcompl_ref)=c("x","y")
proj4string(DataGCLRcompl_ref) <- CRS("+init=epsg:4326") #WGS84
DataGCLRcompl_L93=spTransform(DataGCLRcompl_ref,CRS("+init=epsg:2154"))
test=coordinates(DataGCLRcompl_L93)
DataGCLRcompl$x=test[,1]
DataGCLRcompl$y=test[,2]


#compile data gite GCLR
DataGCLR=rbindlist(list(DataGCLR,DataGCLRcompl),use.names=T,fill=T)

testN=match(DataGCLR$nom_latin,Batlist$`Scientific name`)
table(subset(DataGCLR$nom_latin,is.na(testN)))

DataGCLR$nom_latin[DataGCLR$nom_latin=="Myotis myotis / Myotis blythii"]=
  "Myotis cf. myotis"
DataGCLR$nom_latin[DataGCLR$nom_latin=="Myotis escalerai"]=
  "Myotis nattereri"
DataGCLR$nom_latin[DataGCLR$nom_latin=="Myotis crypticus"]=
  "Myotis nattereri"
DataGCLR$nom_latin[DataGCLR$nom_latin=="Myotis nattereri / Myotis escalerai"]=
  "Myotis nattereri"
DataGCLR$nom_latin[DataGCLR$nom_latin=="Rhinolophus mehelyi / Rhinolophus euryale"]=
  "Rhinolophus euryale"



table(DataGCLR$contact)
DataGCLR=subset(DataGCLR,DataGCLR$contact %in% c("Témoignages"   ,"Vu"))
                                                 
DataGCLR$x=round(DataGCLR$x,-2)
DataGCLR$y=round(DataGCLR$y,-2)
DataGCLR$yday=yday(dmy(DataGCLR$date_debut))
DataGCLR$diffyday=abs(DataGCLR$yday-yDateT)
testS=match(DataGCLR$nom_latin,SpeciesList$`Scientific name`)
table(subset(DataGCLR$nom_franc,is.na(testS)))
DataGCLR$espece=SpeciesList$Esp[testS]
fwrite(DataGCLR,paste0("C:/Users/yvesb/Documents/www/GCLRGitesPeriod",DateT,".csv"),sep=";")


ClosestDateG=aggregate(DataGCLR$diffyday
                       ,by=c(list(DataGCLR$x)
                             , list(DataGCLR$y)), min)
testG=match(paste(DataGCLR$x,DataGCLR$y,DataGCLR$diffyday)
            ,paste(ClosestDateG$Group.1,ClosestDateG$Group.2
                   ,ClosestDateG$x))
DataGCLRPeriod=subset(DataGCLR,!is.na(testG))

#purge
#BatNuitP=BatNuitPeriodC[0,]
BatNuitP=data.frame()
for (i in 1:length(unique(BatNuitPeriodC$espece)))
{
  iNuit=subset(BatNuitPeriodC
               ,BatNuitPeriodC$espece==unique(BatNuitPeriodC$espece)[i])
  if(!is.na(iNuit$Radius[1])){
    
    coordinates(iNuit)=c("longitude","latitude")
    proj4string(iNuit) <- CRS("+init=epsg:4326") # WGS 84
    
    
    
    iGiteVC=subset(TrueGiteP
                   ,TrueGiteP$espece==unique(BatNuitPeriodC$espece)[i])
    if(nrow(iGiteVC)>0){
      coordinates(iGiteVC)=c("longitude","latitude")
      proj4string(iGiteVC) <- CRS("+init=epsg:4326") # WGS 84
      
      buff1=buffer(iGiteVC,width=iNuit$Radius[1]*1000)
      plot(buff1)
      test=over(iNuit,buff1) 
      # iNuit$gite1=(!is.na(test))
      #spplot(iNuit,zcol="gite1")
      #iNuit2=subset(iNuit,is.na(test))
      iNuit$ProcheGite=(!is.na(test))
    }else{
      #iNuit2=iNuit
      iNuit$ProcheGite=F
      test=match("5f219d3f2e4b10000f6c71a8",iNuit$participation)
      
    }
    
    iGCLR=subset(DataGCLRPeriod,DataGCLRPeriod$espece==unique(BatNuitPeriodC$espece)[i])
    if(nrow(iGCLR)>0)
    {
      coordinates(iGCLR)=c("x","y")
      proj4string(iGCLR) <- CRS("+init=epsg:2154") # L93
      iGCLR=spTransform(iGCLR,CRS("+init=epsg:4326"))
      buff2=buffer(iGCLR,width=iNuit$Radius[1]*1000)
      plot(buff2)
      test3=over(iNuit,buff2) 
      # iNuit$gite1=(!is.na(test))
      #spplot(iNuit,zcol="gite1")
      #iNuit3=subset(iNuit2,is.na(test3))
      iNuit$ProcheGite=ifelse(iNuit$ProcheGite,iNuit$ProcheGite,(!is.na(test3)))
    summary(iNuit$ProcheGite)
      }#else{
    #  iNuit3=iNuit2
    #}
    
    testF=over(iNuit,Fw84)
    
    iNuit4=subset(iNuit,testF$NOM_RégIO=="LANGUEDOC-ROUSSILLON")  
    dim(iNuit)
    dim(iNuit4)    
    BatNuitP=rbind(BatNuitP,as.data.frame(iNuit4))
  }
}

#maternities
BatNuitM=data.frame()
for (i in 1:length(unique(BatNuitPeriodC$espece)))
{
  Li=subset(ListeBats,ListeBats$Espece==unique(BatNuitPeriodC$espece)[i])
  if(nrow(Li)>0)
  {
    Li_mb=yday(Li$`Mise-bas`[1])
    if(Li_mb<yDateT)
    {
      #stop("recoder mise-bas avec tag ProcheGite")
      #Li_endrepro=yday(Li$`Depart gite mb`[1])
      Li_endrepro=Li_mb+45
      if(Li_endrepro>yDateT)
      {
        
        iNuit=subset(BatNuitPeriodC
                     ,BatNuitPeriodC$espece==unique(BatNuitPeriodC$espece)[i])
        test=match("5f219d3f2e4b10000f6c71a8",iNuit$participation)
        
        if(!is.na(iNuit$Radius[1])){
          coordinates(iNuit)=c("longitude","latitude")
          proj4string(iNuit) <- CRS("+init=epsg:4326") # WGS 84
          
          iNuit$IndiceCap=iNuit$IndiceCap*3
          test=match("5f219d3f2e4b10000f6c71a8",iNuit$participation)
          
          iGCLR=subset(DataGCLR,DataGCLR$espece==unique(BatNuitPeriodC$espece)[i])
          iGCLR=subset(iGCLR,!is.na(iGCLR$x))
          if(nrow(iGCLR)>0){
            
            coordinates(iGCLR)=c("x","y")
            proj4string(iGCLR) <- CRS("+init=epsg:2154") # L93
            iGCLR=spTransform(iGCLR,CRS("+init=epsg:4326"))
            
            iRepro=subset(iGCLR,(iGCLR$yday>yday(Li$`Mise-bas`[1]))&
                            (iGCLR$yday<yday(Li$`Depart gite mb`[1])))
            
            iMale=subset(iRepro,iRepro$sexe=="Mâle")
            iFemelle=subset(iRepro,iRepro$sexe=="Femelle")
            iGCLR=subset(iGCLR,iGCLR$MB)
            
          }else{
            iMale=iNuit[0,]
            iFemelle=iNuit[0,]
            
          }
          #data perso
          iperso=subset(DataCapturePerso
                        ,DataCapturePerso$espece==
                          unique(BatNuitPeriodC$espece)[i])
          if(nrow(iperso)>0)
          {
            coordinates(iperso)=c("X","Y")
            proj4string(iperso) <- CRS("+init=epsg:4326") # W84
            iRepro2=subset(iperso,(yday(iperso$Date)>yday(Li$`Mise-bas`[1]))&
                             (yday(iperso$Date)<yday(Li$`Depart gite mb`[1])))
            
            iMale2=subset(iRepro2,iRepro2$Sexe=="M")
            iFemelle2=subset(iRepro2,iRepro2$Sexe=="F")
          }else{
            iMale2=iMale[0,]
            iFemelle2=iFemelle[0,]
            
          }
          
          if(nrow(iGCLR)>0)
          {
            buff2=buffer(iGCLR,width=iNuit$Radius[1]*1000)
            
            plot(buff2)
            test3=over(iNuit,buff2) 
            # iNuit$gite1=(!is.na(test))
            #spplot(iNuit,zcol="gite1")
            #iNuit3=subset(iNuit,is.na(test3))
            iNuit$ProcheGite=(!is.na(test3))
          }else{
            iNuit$ProcheGite=F
          }
          
          CM1=as.data.frame(coordinates(iMale))
          names(CM1)=c("X","Y")
          CM2=as.data.frame(coordinates(iMale2))
          CM=rbind(CM1,CM2)
          if(nrow(CM)>0)
          {
            coordinates(CM)=c("X","Y")
            proj4string(CM) <- CRS("+init=epsg:4326") # W84
            
            CF1=as.data.frame(coordinates(iFemelle))
            names(CF1)=c("X","Y")
            CF2=as.data.frame(coordinates(iFemelle2))
            CF=rbind(CF1,CF2)
            if(nrow(CF)>0){
              coordinates(CF)=c("X","Y")
              proj4string(CF) <- CRS("+init=epsg:4326") # W84
            }else{
              CF=CM[0,]
            }
            
            if(length(CM)>0)
            {
              buffM=buffer(CM,width=iNuit$Radius[1]*1000)
              if(length(CF)>0)
              {
                buffF=buffer(CF,width=iNuit$Radius[1]*1000)
              }else{
                buffF=buffM[0,]
              }
              plot(buffM)
              plot(buffF)
              test3=over(iNuit,buffM)
              test4=over(iNuit,buffF)
              test34=((is.na(test3))|(!is.na(test4)))
              # iNuit$gite1=(!is.na(test))
              #spplot(iNuit,zcol="gite1")
              iNuit$ProcheGite=ifelse(test34,F,iNuit$ProcheGite)
            }
          }
          testF=over(iNuit,Fw84)
          
          iNuit4=subset(iNuit,testF$NOM_RégIO=="LANGUEDOC-ROUSSILLON")  
          dim(iNuit4)    
          BatNuitM=rbind(BatNuitM,as.data.frame(iNuit4))
        }
      }
    }
  }
}
#swarming

BatNuitS=data.frame()
for (i in 1:length(unique(BatNuitPeriodC$espece)))
{
  Li=subset(ListeBats,ListeBats$Espece==unique(BatNuitPeriodC$espece)[i])
  if(nrow(Li)>0)
  {
    if(Li$`Mois swarming`[1]==month(lDateT)){
      #stop("recoder swarming avec tag ProcheGite")
      iNuit=subset(BatNuitPeriodC
                   ,BatNuitPeriodC$espece==unique(BatNuitPeriodC$espece)[i])
      
      coordinates(iNuit)=c("longitude","latitude")
      proj4string(iNuit) <- CRS("+init=epsg:4326") # WGS 84
      
      iNuit$IndiceCap=iNuit$IndiceCap*2
      if(!is.na(iNuit$Radius[1])){
        
        iSw=subset(SiteSw,SiteSw$Espece==unique(BatNuitPeriodC$espece)[i])
        if(nrow(iSw)>0)
        {
          #stop("code sw")
          coordinates(iSw)=c("longitude","latitude")
          proj4string(iSw) <- CRS("+init=epsg:4326") # WGS84
          #iGCLR=spTransform(iGCLR,CRS("+init=epsg:4326"))
          buff2=buffer(iSw,width=iNuit$Radius[1]*1000)
          plot(buff2)
          test3=over(iNuit,buff2) 
          # iNuit$gite1=(!is.na(test))
          #spplot(iNuit,zcol="gite1")
          iNuit$ProcheGite=(is.na(test3))
        }else{
          iNuit$ProcheGite=F
        }
        
        testF=over(iNuit,Fw84)
        summary(testF)
        
        iNuit4=subset(iNuit,testF$NOM_RégIO=="LANGUEDOC-ROUSSILLON")  
        dim(iNuit4)    
        BatNuitS=rbind(BatNuitS,as.data.frame(iNuit4))
      }
    }
  }
}

BatNuitP$type="gite"
if(nrow(BatNuitM)>0){BatNuitM$type="mb"}
if(nrow(BatNuitS)>0){BatNuitS$type="sw"}

BatNuitAll=rbind(BatNuitP,BatNuitM,BatNuitS)

Site=unique(paste(BatNuitAll$latitude,BatNuitAll$longitude,sep=","))

BatNuitAll$NumSite=match(paste(BatNuitAll$latitude,BatNuitAll$longitude,sep=",")
                         ,Site)

#supprimer les doublons de participation
BatNuitAll=BatNuitAll[order(BatNuitAll$participation,decreasing=T),]

BatDedoublonne=BatNuitAll[0,]
for (i in 1:length(Site)){
  Bati=subset(BatNuitAll,BatNuitAll$NumSite==i)
  Bati=subset(Bati,Bati$participation==Bati$participation[1])
  BatDedoublonne=rbind(BatDedoublonne,Bati)
}

plot(BatNuitAll$longitude,BatNuitAll$latitude)
BatNuitAll$distance=((BatNuitAll$longitude-CoordO[2])^2+
                       ((BatNuitAll$latitude-CoordO[1])*120/85)^2)^0.5*80
summary(BatNuitAll$distance)


#recoder en mettant à zéro les ProcheGite==T
BatNuitAll$IndiceCap=ifelse(BatNuitAll$ProcheGite,0,BatNuitAll$IndiceCap)
BatNuitA=aggregate(BatNuitAll$IndiceCap,by=c(list(BatNuitAll$NumSite),list(BatNuitAll$participation)),sum)
BatNuitA=BatNuitA[order(BatNuitA$Group.2,decreasing=T),]
BatNuitA=unique(BatNuitA,by="Group.1")

BatNuitA$CoordSite=Site[BatNuitA$Group.1]
#BatNuitA=BatNuitA[order(BatNuitA$x,decreasing=T),]
testS=match(BatNuitA$Group.1,BatNuitAll$NumSite)
BatNuitA$distance=BatNuitAll$distance[testS]
BatNuitA=BatNuitA[order(BatNuitA$distance),]

fwrite(BatNuitA,paste0("BatNuitA_",DateT,".csv"),sep=";")
fwrite(BatNuitAll,paste0("BatNuitAll_",DateT,".csv"),sep=";")


ScoreMax=0
skip="n"
for (i in 1:nrow(BatNuitA)){
  if(BatNuitA$x[i]>ScoreMax){
    print(i)
    print(ScoreMax)
    print(BatNuitA$Group.1[i])
    print(BatNuitA$distance[i])
    Detailsi=subset(BatNuitAll,BatNuitAll$NumSite==BatNuitA$Group.1[i])
    print(Detailsi$participation[1])
    print(paste0(Detailsi$latitude[1],",",Detailsi$longitude[1]))
    print(subset(Detailsi,select=c("espece","nb_contacts_nd","ProcheGite","type")))
    DetailsiI=subset(Detailsi,Detailsi$IndiceCap>(0.05*max(Detailsi$IndiceCap)))
    print(subset(DetailsiI,select=c("espece","nb_contacts_nd","type","IndiceCap")))
    skip=readline()
    if(skip=="n"){ScoreMax=BatNuitA$x[i]}
    }
}




