library(data.table)
library(bit64)
library(raster)
#library(rgdal)
#library(readxl)
library(sf)

#DataAll=fread("C:/Users/ybas/Documents/VigieChiro/gbifData/Raw/Mtp0031884-230530130749713.csv")
DataAll=fread("C:/Users/ybas/Documents/VigieChiro/gbifData/Raw/Paris_0257474-230224095556074.csv")
#DataAll=fread("C:/Users/ybas/Documents/VigieChiro/gbifData/Raw/DigoinRoanne0141294-230530130749713.csv")
#DataAll=fread("C:/Users/ybas/Documents/VigieChiro/gbifData/Raw/Bourges0016085-231002084531237.csv")
#SelPol=shapefile("./SIG/R12.shp")
#ListTargets=fread("./VigieChiro/gbifData/DataSelDate/DataSelRare__260.csv")
#DirDG="./VigieChiro/gbifData/DataGroup"
#Suffix="Catalogne"
OutputName="C:/Users/ybas/Documents/www/Raw57Paris"
#Groupes=read_xlsx("GroupeSp.xlsx")
Date=57
SpeciesAll=fread("SpeciesAll.csv",h=T,sep=";")
#PhylumList=c("Chordata","Arthropoda","Tracheophyta","Mollusca")
#HeureEte=T
SpHeureAll=fread("SpHeure.csv")

#print(head(SpeciesAll))

dir.create(OutputName)

test0=("Pistacia lentiscus" %in% SpeciesAll$Scientific.name)
test1=subset(SpeciesAll,grepl("Pistacia",SpeciesAll$Scientific.name))

test=(DataAll$species %in% SpeciesAll$Scientific.name)
DataFj=subset(DataAll,!test)
#print("Solanum villosum"  %in% DataFj$name)
#print("Solanum villosum"  %in% SpeciesAll$Scientific.name)
DataFj$yday=DataFj$day+(DataFj$month-1)*30

DataFj=subset(DataFj,DataFj$yday>Date-15)
DataFj=subset(DataFj,DataFj$yday<Date+15)

#DataFj=subset(DataFj,DataFj$phylum %in% PhylumList)
DataFj=subset(DataFj,DataFj$species!="")
DataFj=subset(DataFj,(DataFj$individualCount>0)|(is.na(DataFj$individualCount)))

table(DataFj$phylum)
table(DataFj$class)


if(nrow(DataFj)>0){
  if(!("coordinateUncertaintyInMeters" %in% names(DataFj))){
    DataFj$coordinateUncertaintyInMeters=NA
  }
  
  DataFj=subset(DataFj,!is.na(DataFj$decimalLongitude))
  DataFj=subset(DataFj,!is.na(DataFj$decimalLatitude))
  print(nrow(DataFj))
  print("L68")
  coordinates(DataFj)=c("decimalLongitude","decimalLatitude")
  proj4string(DataFj) <- CRS("+init=epsg:4326") # WGS 84
  print("L71")
  DataPol=DataFj
  
  
  
  
  #DataPol=raster::intersect(DataFj,SelPol)
  if(nrow(DataPol)>0){
    print("L75")
    #determiner les heures
    ListSpNew=unique(DataPol$species)
    Heure=vector()
    for (y in 1:length(ListSpNew)){
      
      Datay=subset(DataAll,DataAll$species==ListSpNew[y])
      Hy=hour(Datay$eventDate)#+1+HeureEte
      My=minute(Datay$eventDate)
      Sy=second(Datay$eventDate)
      HyReal=subset(Hy,My!=0|Sy!=0)
      DatayH=subset(Datay,My!=0|Sy!=0)
      
      if(length(HyReal)==0){
        Heure=c(Heure,"H")
      }
      if(length(HyReal)==1){
        Heurey=paste0(HyReal-2,"H",HyReal+3)
        Heure=c(Heure,Heurey)
      }
      # if(length(HyReal)==2){
      #   stop("code 2A")
      #   Heurey=paste0(HyReal-2,"H",HyReal+2)
      #   Heure=c(Heure,Heurey)
      # }
      # 
      if(length(HyReal)>1){
        #stop("code")
        Monthy=month(DatayH$eventDate)
        if(length(HyReal)==2){
          HyRestricted=HyReal
        }else{
          HyRestricted=subset(HyReal,Monthy==ceiling(Date/30))
          if(length(HyRestricted)<3){
            HyRestricted=HyReal
          }
        }
        if(length(HyRestricted)>1){
          # Prop1=0
          # Prop2=0
          # Prop=0
          TabH=as.data.frame(table(HyRestricted))
          TabH=TabH[order(TabH$Freq,decreasing=T),]
          TabH$CS=cumsum(TabH$Freq)
          SelMax=min(which(TabH$CS>max(TabH$CS)*0.67))
          HourSel=as.numeric(as.character(TabH$HyRestricted[1:SelMax]))
          HourSel2=ifelse(HourSel<12,HourSel+24
                          ,HourSel)
          Start1=min(HourSel)
          End1=max(HourSel)
          Start2=min(HourSel2)
          End2=max(HourSel2)
          Range1=End1-Start1
          Range2=End2-Start2
          if(Range1<Range2){
            Heurey=paste0(Start1-(Start1>23)*24,"H",End1-(End1>22)*24+1)
          }else{
            Heurey=paste0(Start2-(Start2>23)*24,"H",End2-(End2>22)*24+1)
          }
          Heure=c(Heure,Heurey)  
          
          
          #   
          #   Start1=floor(quantile(HyRestricted,0.125))
          #   End1=ceiling(quantile(HyRestricted,0.875))
          #   HyNight=HyRestricted+(HyRestricted<=12)*24
          #   Start2=floor(quantile(HyNight,0.125))
          #   End2=ceiling(quantile(HyNight,0.875))
          #   if((End1-Start1)<(End2-Start2)){
          #     if(length(HyRestricted)==2){
          #       Start1=Start1-1
          #       End1=End1+1
          #     }
          #     Heurey=paste0(Start1,"H",End1+1)
          #     Heure=c(Heure,Heurey)
          #   }else{
          #     if(length(HyRestricted)==2){
          #       Start2=Start2-1
          #       End2=End2+1
          #     }
          # Heurey=paste0(Start2-(Start2>23)*24,"H",End2-(End2>22)*24+1)
          # Heure=c(Heure,Heurey)
          # 
          
          
        }else{
          stop("code 0B")
        }
        
      }
      if(y%%300==1){print(paste(y,Sys.time(),ListSpNew[y],Heure[length(Heure)]))}      
    }
    
  }
  
  SpHeure=data.frame(ListSpNew,Heure)
  
  testH=match(DataPol$species,SpHeure$ListSpNew)
  testH2=match(DataPol$species,SpHeureAll$ListSpNew)
  
  HeureHere=ifelse(SpHeure$Heure[testH]=="H",SpHeureAll$Heure[testH2],SpHeure$Heure[testH])
  table(substr(HeureHere,1,2))
  
  PPdf=as.data.frame(DataPol)
  #EspNew=unique(PPdf$name)
  print("L78")
  DataPol$label=paste(DataPol$species,DataPol$coordinateUncertaintyInMeters,HeureHere)
  DataPol$Heure=HeureHere
  print("L80")
  DataPol$HeureDeb=tstrsplit(DataPol$Heure,split="H")[[1]]
  table(DataPol$HeureDeb)
  DataPol$HeureFin=tstrsplit(DataPol$Heure,split="H")[[2]]
  table(DataPol$HeureFin)
  DataPol$Nuit=((as.numeric(DataPol$HeureDeb)<6)|
                  (as.numeric(DataPol$HeureFin)<as.numeric(DataPol$HeureDeb))|
                  (as.numeric(DataPol$HeureFin)>24)|
                  DataPol$HeureDeb==""
  )
  DataPol$Matin=((as.numeric(DataPol$HeureDeb)<12)&(as.numeric(DataPol$HeureDeb)>5)|
                   (as.numeric(DataPol$HeureFin)<13)&(as.numeric(DataPol$HeureFin)>6)|
                   (as.numeric(DataPol$HeureFin)>6)&(as.numeric(DataPol$HeureDeb)<12)|
                   DataPol$HeureDeb==""
  )
  DataPol$Aprem=((as.numeric(DataPol$HeureDeb)<18)&(as.numeric(DataPol$HeureDeb)>11)|
                   (as.numeric(DataPol$HeureFin)<19)&(as.numeric(DataPol$HeureFin)>12)|
                   (as.numeric(DataPol$HeureFin)>12)&(as.numeric(DataPol$HeureDeb)<18)|
                   DataPol$HeureDeb==""
  )
  DataPol$Soir=((as.numeric(DataPol$HeureDeb)<24)&(as.numeric(DataPol$HeureDeb)>17)|
                  (as.numeric(DataPol$HeureFin)<25)&(as.numeric(DataPol$HeureFin)>18)|
                  (as.numeric(DataPol$HeureFin)>18)&(as.numeric(DataPol$HeureDeb)<24)|
                  (as.numeric(DataPol$HeureDeb)<0)|
                  ((as.numeric(DataPol$HeureFin)<as.numeric(DataPol$HeureDeb))&
                     (as.numeric(DataPol$HeureDeb)<18))|
                  DataPol$HeureDeb==""
  )
  
  table(DataPol$Matin,DataPol$HeureDeb)
  table(DataPol$Nuit,DataPol$HeureDeb)
  table(DataPol$Soir,DataPol$HeureDeb)
  test=subset(HeureHere,(DataPol$HeureDeb=="17"&!DataPol$Soir))
  
  DataPrecise=subset(DataPol,DataPol$coordinateUncertaintyInMeters<1000)
  table(DataPrecise$phylum)[order(table(DataPrecise$phylum),decreasing=T)]
  PPNA=subset(DataPol,is.na(DataPol$coordinateUncertaintyInMeters))
  DataPT=rbind(DataPrecise,PPNA)
  print(head(table(DataPT$phylum)[order(table(DataPT$phylum),decreasing=T)]))
  print(head(table(DataPT$class)[order(table(DataPT$class),decreasing=T)]))
  print(head(table(DataPT$order)[order(table(DataPT$order),decreasing=T)]))
  print(head(table(DataPT$family)[order(table(DataPT$family),decreasing=T)]))
  print(head(table(DataPT$genus)[order(table(DataPT$genus),decreasing=T)]))
  print(head(table(DataPT$species)[order(table(DataPT$species),decreasing=T)]))
  #for (k in 1:ceiling(nrow(PPdf)/100000)){
  #print(k)
  #Datak=DataPol[((k-1)*100000+1):(min(nrow(DataPol),k*100000)),]
  
  
  for (k in 1:length(unique(DataPT$phylum))){
    Datak=subset(DataPT,DataPT$phylum==unique(DataPT$phylum)[k])
    #print(unique(DataPT$phylum)[k])
    ##print(dim(Datak))
    Datak1=subset(Datak,Datak$Nuit)
    ##print(dim(Datak1))
    if(nrow(Datak1)>0){
      
      Datak1=subset(Datak1,select=c("species","month","year","day","yday"
                                    ,"coordinateUncertaintyInMeters","individualCount","label"))
      st_write(st_as_sf(Datak1),
               dsn = paste0(OutputName, "/", basename(OutputName), "_Nuit_", unique(DataPT$phylum)[k], ".geojson"),
               driver = "GeoJSON",
               overwrite = TRUE, append=FALSE)
    }
    Datak1=subset(Datak,Datak$Matin)
    #print(dim(Datak1))
    if(nrow(Datak1)>0){
      
      Datak1=subset(Datak1,select=c("species","month","year","day","yday"
                                    ,"coordinateUncertaintyInMeters","individualCount","label"))
      st_write(st_as_sf(Datak1),
               dsn = paste0(OutputName, "/", basename(OutputName), "_Matin_", unique(DataPT$phylum)[k], ".geojson"),
               driver = "GeoJSON",
               overwrite = TRUE, append=FALSE)
      
    }
    
    Datak1=subset(Datak,Datak$Aprem)
    if(nrow(Datak1)>0){
      Datak1=subset(Datak1,select=c("species","month","year","day","yday"
                                    ,"coordinateUncertaintyInMeters","individualCount","label"))
      
      st_write(st_as_sf(Datak1),
               dsn = paste0(OutputName, "/", basename(OutputName), "_Aprem_", unique(DataPT$phylum)[k], ".geojson"),
               driver = "GeoJSON",
               overwrite = TRUE, append=FALSE)
    }
    
    Datak1=subset(Datak,Datak$Soir)
    if(nrow(Datak1)>0){
      Datak1=subset(Datak1,select=c("species","month","year","day","yday"
                                    ,"coordinateUncertaintyInMeters","individualCount","label"))
      
      st_write(st_as_sf(Datak1),
               dsn = paste0(OutputName, "/", basename(OutputName), "_Soir_", unique(DataPT$phylum)[k], ".geojson"),
               driver = "GeoJSON",
               overwrite = TRUE, append=FALSE)
    }
    #}  
  }
  
  DataA=subset(DataPT,DataPT$phylum=="Arthropoda")
  dim(DataA)
  for (k in 1:length(unique(DataA$order))){
    Datak=subset(DataA,DataA$order==unique(DataA$order)[k])
    Datak=subset(Datak,select=c("species","month","year","day","yday"
                                ,"coordinateUncertaintyInMeters","individualCount","label"))
    # writeOGR(Datak, dsn=paste0(OutputName,"/",basename(OutputName),"_OA_",unique(DataA$order)[k],".geojson")
    #          , layer=OutputName, driver="GeoJSON",overwrite=T)
    st_write(st_as_sf(Datak), 
             dsn = paste0(OutputName, "/", basename(OutputName), "_OA_", unique(DataA$order)[k], ".geojson"),
             driver = "GeoJSON", 
             overwrite = TRUE, append=FALSE)
    #}  
  }
  
  
  #PMaxtot=vector()
  #for (k in 1:length(EspNew))
  #{
  #PP10=subset(DataPol,DataPol$coordinateUncertaintyInMeters<10)
  #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
  #if(nrow(PP10)>0){
  
  #writeOGR(PP10, dsn=paste0(OutputName,"/Raw",GroupeL[j],"10_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
  #print(paste0(OutputName,"/Raw",GroupeL[j],"10_RawData.geojson"))
  #}
  #PP100=subset(DataPol,(DataPol$coordinateUncertaintyInMeters<100)&(DataPol$coordinateUncertaintyInMeters>=10))
  #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
  #if(nrow(PP100)>0){
  
  # writeOGR(PP100, dsn=paste0(OutputName,"/Raw",GroupeL[j],"100_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
  #}
  #PP1000=subset(DataPol,(DataPol$coordinateUncertaintyInMeters<1000)&(DataPol$coordinateUncertaintyInMeters>=100))
  #PPk=unique(as.data.table(PPk),by=c("decimalLongitude","decimalLatitude"))
  # if(nrow(PP1000)>0){
  
  #  writeOGR(PP1000, dsn=paste0(OutputName,"/Raw",GroupeL[j],"1000_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
  #}
  #PPNA=subset(DataPol,is.na(DataPol$coordinateUncertaintyInMeters))
  #if(nrow(PPNA)>0){
  
  # writeOGR(PPNA, dsn=paste0(OutputName,"/Raw",GroupeL[j],"NA_RawData.geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
  # print(paste0(OutputName,"/Raw",GroupeL[j],"NA_RawData.geojson"))
  #}
  
  
  
}



