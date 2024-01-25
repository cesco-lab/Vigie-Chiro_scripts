library(readxl)
library(data.table)
library(raster)
library(lubridate)
library(rgdal)

SpTarget="Minsch"
DiffDay=30
DateT="2023-11-01"
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
Fdep=shapefile("C:/Users/yvesb/Documents/SIG/Limite_administrative/France_dep_L93.shp")
CoordO=c(43.826,3.77) # LD
#CoordO=c(43.522026333413436, 2.830690500495964) #Premian
#CoordO=c(42.62579592335401, 2.0980440005120045) # Espousouille
#CoordO=c(43.93493682074521, 4.3207776) #sainte-anastasie

DataCapturePerso=fread("C:/Users/yvesb/Documents/chiros/DataAllCapture.csv")

Fw84=spTransform(Fdep,CRS("+init=epsg:4326"))

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

iNuit=subset(BatNuitPeriodC
             ,BatNuitPeriodC$espece==SpTarget)
if(!is.na(iNuit$Radius[1])){
  
  coordinates(iNuit)=c("longitude","latitude")
  proj4string(iNuit) <- CRS("+init=epsg:4326") # WGS 84
  
  
  
  iGiteVC=subset(TrueGiteP
                 ,TrueGiteP$espece==SpTarget)
  iGCLR=subset(DataGCLRPeriod,DataGCLRPeriod$espece==SpTarget)
  
  if(nrow(iGiteVC)>0){
    coordinates(iGiteVC)=c("longitude","latitude")
    proj4string(iGiteVC) <- CRS("+init=epsg:4326") # WGS 84
    
    buff1=buffer(iGiteVC,width=iNuit$Radius[1]*1000)
    plot(buff1)
    buff1$type="VC"
    #writeOGR(obj=buff1, dsn=".", layer=paste0(SpTarget,"_",yDateT,"_gitesVC"), driver="ESRI Shapefile") 
    #test=over(iNuit,buff1) 
    # iNuit$gite1=(!is.na(test))
    #spplot(iNuit,zcol="gite1")
    #iNuit2=subset(iNuit,is.na(test))
    if(nrow(iGCLR)>0)
    {
      coordinates(iGCLR)=c("x","y")
      proj4string(iGCLR) <- CRS("+init=epsg:2154") # L93
      iGCLR=spTransform(iGCLR,CRS("+init=epsg:4326"))
      buff2=buffer(iGCLR,width=iNuit$Radius[1]*1000)
      plot(buff2)
      buff2$type="GCLR"
      buffAll=rbind(buff1,buff2)
      writeOGR(obj=buffAll, dsn=".", layer=paste0(SpTarget,"_",yDateT,"_buffer")
               , driver="ESRI Shapefile") 
      writeOGR(obj=iGCLR, dsn=".", layer=paste0(SpTarget,"_",yDateT,"_gitesgclr")
               , driver="ESRI Shapefile") 
      iGiteVC$Nuit=NULL
      writeOGR(obj=iGiteVC, dsn=".", layer=paste0(SpTarget,"_",yDateT,"_gitesvc")
               , driver="ESRI Shapefile") 
      
            
    }else{
      writeOGR(obj=buff1, dsn=".", layer=paste0(SpTarget,"_",yDateT,"_buffer")
               , driver="ESRI Shapefile") 
      writeOGR(obj=iGiteVC, dsn=".", layer=paste0(SpTarget,"_",yDateT,"_gitesvc")
               , driver="ESRI Shapefile") 
      
    }
  }else{
    if(nrow(iGCLR)>0)
    {
      coordinates(iGCLR)=c("x","y")
      proj4string(iGCLR) <- CRS("+init=epsg:2154") # L93
      iGCLR=spTransform(iGCLR,CRS("+init=epsg:4326"))
      buff2=buffer(iGCLR,width=iNuit$Radius[1]*1000)
      plot(buff2)
      buff2$type="GCLR"
      writeOGR(obj=buff2, dsn=".", layer=paste0(SpTarget,"_",yDateT,"_buffer")
               , driver="ESRI Shapefile") 
      writeOGR(obj=iGCLR, dsn=".", layer=paste0(SpTarget,"_",yDateT,"_gitespoints")
               , driver="ESRI Shapefile") 
      
    }
  }
}
