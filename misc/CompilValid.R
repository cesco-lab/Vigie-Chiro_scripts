library(data.table)
library(readxl)

FValid=list.files("./Tadarida/rounds/test"
                  ,full.names=T)
SpeciesList=fread("SpeciesList.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
DataRP=fread("DataLP_RP.csv")


extension=substr(basename(FValid),nchar(basename(FValid))-3
                 ,nchar(basename(FValid)))
table(extension)
FXL=subset(FValid,extension=="xlsx")
FXL=subset(FXL,substr(basename(FXL),1,1)!="~")
Fcsv=subset(FValid,(extension==".csv")|(extension==".txt"))

XLSlist=list()
for (i in 1:length(FXL))
{
  XLSlist[[i]]=read_excel(FXL[i])
  XLSlist[[i]]$FICHIER=basename(FXL[i])
  ValidGood=("validateur_taxon" %in% names(XLSlist[[i]]))&
    ("validateur_probabilite" %in% names(XLSlist[[i]]))
  ObsGood=("observateur_taxon" %in% names(XLSlist[[i]]))&
    ("observateur_probabilite" %in% names(XLSlist[[i]]))
  if(!ValidGood&!ObsGood)
  {
    
    print(basename(FXL[i]))
    print(names(XLSlist[[i]]))
  }  
}
dataXL=rbindlist(XLSlist,fill=T,use.names=T)


if(length(Fcsv)>0)
{
  csvlist=list()
  for (i in 1:length(Fcsv))
  {
    csvlist[[i]]=fread(Fcsv[i])
    test=subset(csvlist[[i]],csvlist[[i]]$Group.1=="Cir48-2013-Pass1-Tron1-Chiro_1_00000_000.wav")
    csvlist[[i]]$FICHIER=basename(Fcsv[i])
    
    if("obs.espece" %in% names(csvlist[[i]]))
    {csvlist[[i]]$observateur_taxon=csvlist[[i]]$obs.espece}
    
    if("obs.proba" %in% names(csvlist[[i]]))
    {csvlist[[i]]$observateur_probabilite=csvlist[[i]]$obs.proba}
    
    if("valid.espece" %in% names(csvlist[[i]]))
    {
      csvlist[[i]]$validateur_taxon=csvlist[[i]]$valid.espece
    }else{
      if(!("validateur_taxon" %in% names(csvlist[[i]]))){
        csvlist[[i]]$validateur_taxon=csvlist[[i]]$Group.2
      }
    }
    
    if("valid.proba" %in% names(csvlist[[i]]))
    {
      csvlist[[i]]$validateur_probabilite=csvlist[[i]]$valid.proba
    }else{
      if(!("validateur_probabilite" %in% names(csvlist[[i]]))){
        csvlist[[i]]$validateur_probabilite=csvlist[[i]]$x
      }
    }
    
    ValidGood=("validateur_taxon" %in% names(csvlist[[i]]))&
      ("validateur_probabilite" %in% names(csvlist[[i]]))
    ObsGood=("observateur_taxon" %in% names(csvlist[[i]]))&
      ("observateur_probabilite" %in% names(csvlist[[i]]))
    if(!ValidGood&!ObsGood)
    {
      print(basename(Fcsv[i]))
      print(names(csvlist[[i]]))
    }  
  }
  datacsv=rbindlist(csvlist,fill=T,use.names=T)
  test=subset(datacsv,datacsv$`nom du fichier`=="Cir48-2013-Pass1-Tron1-Chiro_1_00000_000")
  datatot=rbind(datacsv,dataXL,fill=T,use.names=T)
}else{
  datatot=dataXL
}
datavalid=subset(datatot,(datatot$observateur_taxon!="")|
                   (datatot$validateur_taxon!=""))

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

datavalid$observateur_taxon=ifelse(nchar(datavalid$observateur_taxon)==6,
                                   firstup(datavalid$observateur_taxon),datavalid$observateur_taxon)
datavalid$observateur_taxon[datavalid$observateur_taxon=="aves"]="piaf"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Parasi"]="noise"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Orthoptere"]="Ortsp."
datavalid$observateur_taxon[datavalid$observateur_taxon=="Pleaus/mac"]="Plesp"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Pleaus/Plemac"]="Plesp"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Pipkuhl/nath"]="Pipsp"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Pipkuh/nath"]="Pipsp"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Piaf"]="piaf"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Myotis"]="Myosp"
datavalid$observateur_taxon[datavalid$observateur_taxon=="myomys/myobra"]="Myosp"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Myodau ou mys"]="Myosp"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Grenouille verte"]="amph"
datavalid$observateur_taxon[datavalid$observateur_taxon=="grenouille verte"]="amph"
datavalid$observateur_taxon[datavalid$observateur_taxon=="mydau"]="Myodau"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Epnil"]="Eptnil"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Chiroptera"]="Chirosp"
datavalid$observateur_taxon[datavalid$observateur_taxon=="Myomyo"]="MyoGT"
datavalid$observateur_taxon[datavalid$observateur_taxon=="buzz"]=""


test=subset(datavalid$observateur_taxon,
            !(datavalid$observateur_taxon %in% SpeciesList$Esp))
table(test)
datavalid$observateur_taxon[
  !(datavalid$observateur_taxon %in% SpeciesList$Esp)]=
  ""
test=subset(datavalid$observateur_taxon,
            !(datavalid$observateur_taxon %in% SpeciesList$Esp))
table(test)

table(datavalid$observateur_taxon)[order(table(datavalid$observateur_taxon))]
datavalid$observateur_probabilite[
  datavalid$observateur_taxon==""]=""
table(datavalid$observateur_taxon[
  (datavalid$observateur_taxon!="")&(datavalid$observateur_probabilite=="")])
table(datavalid$FICHIER[
  (datavalid$observateur_taxon!="")&(datavalid$observateur_probabilite=="")])
datavalid$observateur_probabilite[
  (datavalid$observateur_taxon!="")&(datavalid$observateur_probabilite=="")]="POSSIBLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="?"]="POSSIBLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="0"]="SUR"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="1"]="SUR"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="10"]="PROBABLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="15"]="POSSIBLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="20"]="POSSIBLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="25"]="POSSIBLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="30"]="POSSIBLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="40"]="POSSIBLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="5"]="PROBABLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="50"]="POSSIBLE"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="ok"]="SUR"
datavalid$observateur_probabilite[
  datavalid$observateur_probabilite=="OK"]="SUR"

datavalid$observateur_probabilite=toupper(datavalid$observateur_probabilite)

table(datavalid$observateur_probabilite,(datavalid$observateur_taxon==""))


#validations
datavalid$validateur_taxon[datavalid$validateur_taxon=="Mmasp"]="Mimasp"


test=subset(datavalid$validateur_taxon,
            !(datavalid$validateur_taxon %in% SpeciesList$Esp))
table(test)
datavalid$validateur_taxon[
  !(datavalid$validateur_taxon %in% SpeciesList$Esp)]=
  ""
test=subset(datavalid$validateur_taxon,
            !(datavalid$validateur_taxon %in% SpeciesList$Esp))
table(test)

table(datavalid$validateur_taxon)[order(table(datavalid$validateur_taxon))]
datavalid$validateur_probabilite[
  datavalid$validateur_taxon==""]=""
datavalid$validateur_probabilite[
  datavalid$validateur_probabilite=="1"]="POSSIBLE"
datavalid$validateur_probabilite[
  datavalid$validateur_probabilite=="2"]="PROBABLE"
datavalid$validateur_probabilite[
  datavalid$validateur_probabilite=="3"]="SUR"
datavalid$validateur_probabilite[
  datavalid$validateur_probabilite=="4"]="SUR"
datavalid$validateur_probabilite[
  datavalid$validateur_probabilite=="5"]="SUR"

table(datavalid$validateur_taxon[
  (datavalid$validateur_taxon!="")&(datavalid$validateur_probabilite=="")])
table(datavalid$FICHIER[
  (datavalid$validateur_taxon!="")&(datavalid$validateur_probabilite=="")])
datavalid$validateur_probabilite[
  (datavalid$validateur_taxon!="")&(datavalid$validateur_probabilite=="")]="POSSIBLE"
ListTaxa=unique(c(datavalid$validateur_taxon,datavalid$observateur_taxon))[
  order(unique(c(datavalid$validateur_taxon,datavalid$observateur_taxon)))]
LT2=unique(SpeciesList$Nesp2[match(ListTaxa,SpeciesList$Esp)])
LT2[order(LT2)]
OT=SpeciesList$Nesp2[match(datavalid$observateur_taxon,SpeciesList$Esp)]
table(OT)
datavalid$observateur_taxon=OT
VT=SpeciesList$Nesp2[match(datavalid$validateur_taxon,SpeciesList$Esp)]
table(VT)
datavalid$validateur_taxon=VT

#fill donnee value
datavalid[is.na(datavalid)]=""
datavalid$donnee=ifelse((datavalid$donnee)=="",datavalid$`nom du fichier`
                        ,datavalid$donnee)
datavalid$donnee[is.na(datavalid$donnee)]=""
datavalid$donnee=ifelse((datavalid$donnee)=="",datavalid$Filename
                        ,datavalid$donnee)
datavalid$donnee[is.na(datavalid$donnee)]=""
datavalid$donnee=ifelse((datavalid$donnee)=="",datavalid$enregistrements
                        ,datavalid$donnee)
datavalid$donnee=ifelse((datavalid$donnee)=="",datavalid$Group.1
                        ,datavalid$donnee)

table(datavalid$FICHIER[is.na(datavalid$donnee)])
#table(datavalid$Group.1[is.na(datavalid$donnee)])
table(substr(datavalid$donnee,nchar(datavalid$donnee)-3,nchar(datavalid$donnee)-3)
)
subset(datavalid,substr(datavalid$donnee,nchar(datavalid$donnee)-3,nchar(datavalid$donnee)-3)
=="0")
datavalid$donnee=ifelse(substr(datavalid$donnee,nchar(datavalid$donnee)-3,nchar(datavalid$donnee)-3)=="."
                        ,substr(datavalid$donnee,1,nchar(datavalid$donnee)-4),datavalid$donnee)

#dedoublonnage
datavalidShort=subset(datavalid,select=c("participation","donnee","espece"
                                         ,"observateur_taxon"
                                         ,"observateur_probabilite"
                                         ,"validateur_taxon"
                                         ,"validateur_probabilite"
                                         ,"FICHIER"))
datavalidShort[is.na(datavalidShort)]=""
datavalidShort=unique(datavalidShort,by=c("participation","donnee","espece"
                                          ,"observateur_taxon"
                                          ,"observateur_probabilite"
                                          ,"validateur_taxon"
                                          ,"validateur_probabilite"))

#fill missing participation value
datavalidShort$participation[datavalidShort$participation=="(vide)"]=""
datapart=subset(datavalidShort,datavalidShort$participation !="")
dataMP=subset(datavalidShort
              ,datavalidShort$participation =="")
dataMP_PF=subset(dataMP,substr(dataMP$donnee,1,3)=="Car")
dataMP_PF$site=tstrsplit(dataMP_PF$donnee,split="-")[[1]]
dataMP_PF$site=gsub("Car","Vigiechiro - Point Fixe-"
                    ,dataMP_PF$site)
dataMP_PF$point=tstrsplit(dataMP_PF$donnee,split="-")[[4]]
dataMP_PF$date=paste0(substr(dataMP_PF$donnee,nchar(dataMP_PF$donnee)-12
                             ,nchar(dataMP_PF$donnee)-11),"/"
                      ,substr(dataMP_PF$donnee,nchar(dataMP_PF$donnee)-14
                              ,nchar(dataMP_PF$donnee)-13),"/"
                      ,substr(dataMP_PF$donnee,nchar(dataMP_PF$donnee)-18
                              ,nchar(dataMP_PF$donnee)-15))
dataMP_PF$date=as.Date(dataMP_PF$date,tryFormats = c("%d/%m/%Y"))
Particip$dateref=substr(Particip$date_debut,1,10)
Particip$dateref=as.Date(Particip$dateref,tryFormats = c("%d/%m/%Y"))
Particip=Particip[order(Particip$dateref,decreasing=T),]
datacorr=dataMP_PF[0,]
for (i in 1:length(unique(dataMP_PF$site)))
{
  datai=subset(dataMP_PF,dataMP_PF$site==unique(dataMP_PF$site)[i])
  Parti=subset(Particip,Particip$site==unique(dataMP_PF$site)[i])
  print(unique(dataMP_PF$site)[i])
  for (j in 1:length(unique(datai$point)))
  {
    dataj=subset(datai,datai$point==unique(datai$point)[j])
    Partj=subset(Parti,Parti$point==unique(datai$point)[j])
    for (k in 1:nrow(dataj))
    {
      Partk=subset(Partj,Partj$dateref<=dataj$date[k])
      dataj$participation[k]=Partk$participation[1]
    }
    datacorr=rbind(datacorr,dataj)
  }
}
summary(is.na(datacorr$participation))
#head(subset(datacorr$donnee,is.na(datacorr$participation)))
tail(subset(datacorr$donnee,is.na(datacorr$participation)))
#subset(datacorr$donnee,is.na(datacorr$participation))[1000]
datavalidOrphans=subset(datacorr,is.na(datacorr$participation))
fwrite(datavalidOrphans,paste0("datavalidOrphans",Sys.Date(),".csv")
       ,sep=";")
datacorr=subset(datacorr,!is.na(datacorr$participation))
datacorr$site=NULL
datacorr$point=NULL
datacorr$date=NULL


#fill missing participation value for car/pedestrial transects
dataMP_RP=subset(dataMP,substr(dataMP$donnee,1,3)=="Cir")
test=match(dataMP_RP$donnee,DataRP$donnee)
dataMP_RP$participation=DataRP$participation[test]
datavalidOrphansRP=subset(dataMP_RP,is.na(dataMP_RP$participation))
fwrite(datavalidOrphansRP,paste0("datavalidOrphansRP",Sys.Date(),".csv")
       ,sep=";")
dataMP_RP=subset(dataMP_RP,!is.na(dataMP_RP$participation))


#concatenate filled participation tables
datapart=rbind(datapart,datacorr)
datapart=rbind(datapart,dataMP_RP)


fwrite(datapart,paste0("ValidDataPart",Sys.Date(),".csv"),sep=";")
test=subset(datapart,datapart$donnee=="Cir48-2013-Pass1-Tron1-Chiro_1_00000_000")
