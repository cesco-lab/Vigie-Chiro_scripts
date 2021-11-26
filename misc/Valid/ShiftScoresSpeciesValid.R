library(data.table)
library(raster)
library(sf)
library(lubridate)

SpeciesList=fread("SpeciesList.csv")
ValidData=fread("./www/export_validtot210408.txt")
ListPartWavArchiv=fread("./www/wavarchivees.txt")
Particip=fread("./www/p_export.csv",encoding="UTF-8")
SiteLocF="./www/sites_localites.csv"
Zone="C:/Users/yvesb/Documents/SIG/Limite_administrative/France_dep_L93.shp"
SLpred="./VigieChiro/gbifData/SL"
DateSp="./VigieChiro/gbifData/DateSp/"
Nsel=10 #compter un ratio de 1.4 pour obtenir n fichiers
SpIdTot=fread("./www/SpNuit2_0_DataLP_PF_exportTot.csv")
#SpIdTot=fread("DataRP_SpSecteur_0.csv")
RP=F
SelSp=c("Tetvir"
,"Leppun"
        ,"Phogri"
        ,"Phanan"
        ,"Testes"
        ,"Rusnit"
        ,"Plaint"
        ,"Plaalb"
        ,"Yerray"
        ,"Urosp"
        ,"Cyrscu"
        ,"Epheph"
        ,"Isopyr"
        ,"Barfis"
        ,"Antsp"
        ,"Rhasp"
        ,"Plaaff"
        ,"Confus"
        ,"Roeroe"
        ,"Plasab"
        ,"Eupsp"
        ,"Sepsep"
        ,"Tyllil"
        ,"Phofem"
        ,"Metbra"
        ,"Decalb"
        ,"Plafal"
        ,"Phafal"
        ,"Ptepon"
        ,"Antped"
        ,"Condor"
        ,"Pteger"
        ,"Tetcan"
        ,"Bicbic"
        ,"Antius"
        ,"Thycor"
        ,"Urorug"
        ,"Antsor"
        ,"Yerbey"
        ,"Lepbos"
        ,"Lepalb")
SelSp=c("Barbar"
        ,"Hypsav"
        ,"Eptser"
        ,"Nyclei"
        ,"Nycnoc"
        ,"Vesmur"
        ,"Eptnil"
        ,"Nyclas"
        ,"Tadten"
        ,"Pipkuh"
        ,"Pipnat"
        ,"Pippip"
        ,"Pippyg"
        ,"Minsch"
        ,"Rhifer"
        ,"Rhihip"
        ,"Rhieur"
        ,"Pleaur"
        ,"Plemac"
        ,"Pleaus"
        ,"Myodau"
        ,"Myonat"
        ,"Myomys"
        ,"Myoalc"
        ,"Myoema"
        ,"MyoGT")
SelSp=c("Myodau"
        ,"Myonat"
        ,"Myomys"
        ,"Myoalc"
        ,"Myoema"
        ,"MyoGT","Myocap","Myobec","Pipnat","Eptser","Eptnil","Vesmur","Nyclei","Nycnoc","Nyclas","Rhihip","Rhieur"
        ,"Minsch")





Id=aggregate(SpIdTot$score_max,by=c(list(SpIdTot$participation)
                                    ,list(SpIdTot$espece)),max)
#test=match("5f4a3e232baea1000ff00a70",Id$Group.1)
#test=match("5f4a3e232baea1000ff00a70",SpIdTot$participation)

Particip$ProperDate= dmy_hm(Particip$date_debut)
Particip$yday=yday(Particip$ProperDate)
Particip=subset(Particip
                ,Particip$participation %in% ListPartWavArchiv$V1)

SiteLoc=fread(SiteLocF)



DateF=list.files(DateSp,full.names=T)
DateL=list()
for (h in 1:length(DateF))
{
  DateL[[h]]=fread(DateF[h])
}
DateAll=rbindlist(DateL)


SLfiles=list.files(SLpred,full.names=T)

FranceD= shapefile(Zone)
p=st_as_sf(FranceD)
pbuf = st_buffer(p, 1000)
FranceD=as(pbuf,'Spatial')
proj4string(FranceD)=CRS("+init=epsg:2154")

coordinates(SiteLoc)=c("longitude","latitude")
proj4string(SiteLoc)=CRS("+init=epsg:4326")
SiteLoc=spTransform(SiteLoc,CRS("+init=epsg:2154"))

Tri_FR=over(SiteLoc,FranceD)

SiteLoc=fread(SiteLocF)
SL_FR=subset(SiteLoc,!is.na(Tri_FR$CHEF_LIE0))
if(RP)
{
  Part_FRPF=subset(Particip,!substr(Particip$site,1,19)=="Vigiechiro - Point ")
}else{
  

Part_FRPF=subset(Particip,paste(Particip$site,Particip$point)
                 %in% paste(SL_FR$site,SL_FR$nom))
Part_FRPF=subset(Part_FRPF
                 ,substr(Part_FRPF$site,1,19)=="Vigiechiro - Point ")
#Part_RP=subset(Particip,!substr(Particip$site,1,19)=="Vigiechiro - Point ")
#PartFR=c(Part_FRPF$participation,Part_RP$participation)
#test=subset(Part_RP,Part_RP$participation=="5a2aa7ac552423000d77e090")

}

SpeciesSel=subset(SpeciesList,SpeciesList$Group %in% c("bat","bush-cricket"))
SpeciesSel=subset(SpeciesSel,!grepl("sp.",SpeciesSel$`Scientific name`))
SpeciesSel=subset(SpeciesSel,SpeciesSel$France=="x")
SpeciesSel=subset(SpeciesSel,SpeciesSel$Esp %in% SelSp)


#points fixes
ValidFR=subset(ValidData,ValidData$participation %in% Part_FRPF$participation)
SpValidN=aggregate(ValidFR$participation,by=list(ValidFR$valid.espece)
                   ,length)
test=subset(ValidFR,ValidFR$validateur_taxon=="Eptisa")

SpeciesSel2=unique(SpeciesSel,by="Nesp2")
SpeciesSelN=merge(SpeciesSel2,SpValidN,by.x="Nesp2",by.y="Group.1"
                  ,all.x=T)
SpeciesSelN$x[is.na(SpeciesSelN$x)]=0
SpeciesSelN=SpeciesSelN[order(SpeciesSelN$x)]

ValidV=subset(ValidData,ValidData$validateur_taxon!="")
Partdispo=subset(Part_FRPF,!(Part_FRPF$participation %in% ValidV$participation))


#positive shifts
ParSel=vector()
Esp=vector()
icount=0
while(icount<Nsel)
{
  Spi=SpeciesSel[sample(nrow(SpeciesSel),1),]
  print(paste(icount,Spi$Nesp2))
  #Datei=subset(DateAll
  #            ,DateAll$ListSpValide==Spi$`Scientific name`[1])
  #if(nrow(Datei)==0){
  # Datei$ListSpValide[1]=Spi$`Scientific name`[1]
  #Datei$PicSp[1]=sample(c(16:374),1)
  #}
  #Parti=subset(Partdispo,(Partdispo$yday>Datei$PicSp[1]-30)
  #            &(Partdispo$yday<Datei$PicSp[1]+30))
  Parti=Partdispo
  if(RP){
    SL_u=unique(SL_FR,by="site")
  PartSLi=merge(Parti,SL_u,by=c("site"))
  }else{
    PartSLi=merge(Parti,SL_FR,by.x=c("site","point"),by.y=c("site","nom"))
    
  }
  Idi=subset(Id,Id$Group.2==Spi$Nesp2)
  
  SLi=subset(SLfiles,grepl(Spi$`Scientific name`[1]
                           ,basename(SLfiles)))
  if(length(SLi)>0)
  {
    SLpredi=fread(SLi[1])
    
    PartSLpredi=merge(PartSLi,SLpredi,by.x=c("longitude","latitude")
                      ,by.y=c("Group.1","Group.2"))
    
    PartSLpredis=merge(PartSLpredi,Idi,by.x="participation"
                       ,by.y="Group.1")
    PartSLpredis=subset(PartSLpredis,!(PartSLpredis$participation 
                                       %in% ParSel))
    
    if(nrow(PartSLpredis)>0)
    {
      #more than expected
      PartSLpredis$PS=PartSLpredis$x-PartSLpredis$pred
      PartSLpredis=PartSLpredis[sample(nrow(PartSLpredis)),]
      PartSLpredis=PartSLpredis[order(PartSLpredis$PS,decreasing=T),]
      Q75=quantile(PartSLpredis$PS,0.75)
      Max=max(PartSLpredis$PS)
      PartSLpredis$Weights=pmax(0,(PartSLpredis$PS-Q75)/(Max-Q75))
      #PartSLpredis=subset(PartSLpredis,PartSLpredis$Weights>0)
      PartSLpredis1=PartSLpredis[sample(nrow(PartSLpredis),1,prob=PartSLpredis$Weights),]
      ParSeli=PartSLpredis1$participation[1]
      ParSel=c(ParSel,ParSeli)
      Esp=c(Esp,Spi$Nesp2)
      icount=icount+1
    }
  }
}
ParSelFPSpecies=data.frame(cbind(ParSel,Esp))
ParSelFPSpecies$type="FP"




#false negatives tracking
#part 1: missing sp 
ParSel=vector()
Esp=vector()
for (i in 1:round(nrow(ParSelFPSpecies)/2))
{
  
  Spi=subset(SpeciesList,SpeciesList$Esp==ParSelFPSpecies$Esp[i])
  print(paste(i,Spi$Nesp2[1]))
  if(nchar(as.character(ParSelFPSpecies$Esp[i]))<6)
  {
    Spg=subset(SpeciesList,SpeciesList$Nesp2==ParSelFPSpecies$Esp[i])
    Spg=subset(Spg,nchar(Spg$Esp)==6)
  }else{
    Spg=Spi
  }
  Sps=Spg[sample(nrow(Spg),1),]
  
  Datei=subset(DateAll
               ,DateAll$ListSpValide==Sps$`Scientific name`[1])
  if(nrow(Datei)==0){
    Datei=DateAll[1,]
    Datei$ListSpValide[1]=Spi$`Scientific name`[1]
    Datei$PicSp[1]=sample(c(16:374),1)
  }
  Parti=subset(Partdispo,(Partdispo$yday>Datei$PicSp[1]-30)
               &(Partdispo$yday<Datei$PicSp[1]+30))
  #Parti=Partdispo
  if(RP){
    SL_u=unique(SL_FR,by="site")
    PartSLi=merge(Parti,SL_u,by=c("site"))
  }else{
    PartSLi=merge(Parti,SL_FR,by.x=c("site","point"),by.y=c("site","nom"))
    
  }
  
  Idi=subset(Id,Id$Group.2==Spi$Nesp2)
  PartSLi=subset(PartSLi,!(PartSLi$participation %in% Idi$Group.1))
  
  SLi=subset(SLfiles,grepl(Sps$`Scientific name`[1]
                           ,basename(SLfiles)))
  if(length(SLi)>0)
  {
    SLpredi=fread(SLi[1])
    
    PartSLpredi=merge(PartSLi,SLpredi,by.x=c("longitude","latitude")
                      ,by.y=c("Group.1","Group.2"))
    PartSLpredi=PartSLpredi[sample(nrow(PartSLpredi)),]
    PartSLpredi=PartSLpredi[order(PartSLpredi$pred,decreasing=T),]
    PartSLpredi=subset(PartSLpredi
                       ,!(PartSLpredi$participation %in% ParSel))
    #test=match("5f4a3e232baea1000ff00a70",PartSLpredi$participation)
    PartSLpredi=subset(PartSLpredi
                       ,(PartSLpredi$participation %in% Id$Group.1))
    #test=match("5f4a3e232baea1000ff00a70",PartSLpredi$participation)
    #test=match("5f4a3e232baea1000ff00a70",Idi$Group.1)
    
    
    if(nrow(PartSLpredi)>0)
    {
      Q75=quantile(PartSLpredi$pred,0.75)
      Max=max(PartSLpredi$pred)
      PartSLpredi$Weights=pmax(0,(PartSLpredi$pred-Q75)/(Max-Q75))
      #PartSLpredis=subset(PartSLpredis,PartSLpredis$Weights>0)
      PartSLpredis1=PartSLpredi[sample(nrow(PartSLpredi),1,prob=PartSLpredi$Weights),]
      ParSeli=PartSLpredi1$participation[1]
      ParSel=c(ParSel,ParSeli)
      Esp=c(Esp,Spi$Nesp2)
      
    }
  }
  
}
ParSelFNmSpecies=data.frame(cbind(ParSel,Esp))
ParSelFNmSpecies$type="FNm"



#fn part 2: low scores 
ParSel=vector()
Esp=vector()
for (i in (round(nrow(ParSelFPSpecies)/2)+1):nrow(ParSelFPSpecies))
{
  Spi=subset(SpeciesList,SpeciesList$Esp==ParSelFPSpecies$Esp[i])
  print(paste(i,Spi$Nesp2[1]))
  if(nchar(as.character(ParSelFPSpecies$Esp[i]))<6)
  {
    Spg=subset(SpeciesList,SpeciesList$Nesp2==ParSelFPSpecies$Esp[i])
    Spg=subset(Spg,nchar(Spg$Esp)==6)
  }else{
    Spg=Spi
  }
  Sps=Spg[sample(nrow(Spg),1),]
  
  Datei=subset(DateAll
               ,DateAll$ListSpValide==Sps$`Scientific name`[1])
  if(nrow(Datei)==0){
    Datei=DateAll[1,]
    Datei$ListSpValide[1]=Spi$`Scientific name`[1]
    Datei$PicSp[1]=sample(c(16:374),1)
  }
  Parti=subset(Partdispo,(Partdispo$yday>Datei$PicSp[1]-30)
               &(Partdispo$yday<Datei$PicSp[1]+30))
  #Parti=Partdispo
  if(RP){
    SL_u=unique(SL_FR,by="site")
    PartSLi=merge(Parti,SL_u,by=c("site"))
  }else{
    PartSLi=merge(Parti,SL_FR,by.x=c("site","point"),by.y=c("site","nom"))
    
  }
  
  Idi=subset(Id,Id$Group.2==Spi$Nesp2)
  
  
  SLi=subset(SLfiles,grepl(Sps$`Scientific name`[1]
                           ,basename(SLfiles)))
  if(length(SLi)>0)
  {
    SLpredi=fread(SLi[1])
    
    PartSLpredi=merge(PartSLi,SLpredi,by.x=c("longitude","latitude")
                      ,by.y=c("Group.1","Group.2"))
    
    PartSLpredis=merge(PartSLpredi,Idi,by.x="participation"
                       ,by.y="Group.1")
    PartSLpredis=subset(PartSLpredis,!(PartSLpredis$participation 
                                       %in% ParSel))
    
    if(nrow(PartSLpredis)>0)
    {
      #more than expected
      PartSLpredis$PS=PartSLpredis$pred-PartSLpredis$x
      PartSLpredis=PartSLpredis[sample(nrow(PartSLpredis)),]
      PartSLpredis=PartSLpredis[order(PartSLpredis$PS,decreasing=T),]
      Q75=quantile(PartSLpredis$PS,0.75)
      Max=max(PartSLpredis$PS)
      PartSLpredis$Weights=pmax(0,(PartSLpredis$PS-Q75)/(Max-Q75))
      #PartSLpredis=subset(PartSLpredis,PartSLpredis$Weights>0)
      PartSLpredis1=PartSLpredis[sample(nrow(PartSLpredis),1,prob=PartSLpredis$Weights),]
      ParSeli=PartSLpredis1$participation[1]
      ParSel=c(ParSel,ParSeli)
      Esp=c(Esp,Spi$Nesp2)
    }else{
      PartSLpredi=subset(PartSLpredi,!(PartSLpredi$participation 
                                       %in% ParSel))
      PartSLpredi=PartSLpredi[sample(nrow(PartSLpredi)),]
      PartSLpredi=PartSLpredi[order(PartSLpredi$pred,decreasing=T),]
      PartSLpredi=subset(PartSLpredi
                         ,!(PartSLpredi$participation %in% ParSel))
      PartSLpredi=subset(PartSLpredi
                         ,(PartSLpredi$participation %in% Id$Group.1))
      if(nrow(PartSLpredi)>0)
      {
        ParSeli=PartSLpredi$participation[1]
        ParSel=c(ParSel,ParSeli)
        Esp=c(Esp,Spi$Nesp2)
      }     
    }
  }
}
ParSelFNlSpecies=data.frame(cbind(ParSel,Esp))
ParSelFNlSpecies$type="FNl"


ParSelShifts=rbind(ParSelFPSpecies,ParSelFNmSpecies)
ParSelShifts=rbind(ParSelShifts,ParSelFNlSpecies)


fwrite(ParSelShifts,paste0("ParSelShifts",Sys.Date(),".csv")
       ,sep=";")

