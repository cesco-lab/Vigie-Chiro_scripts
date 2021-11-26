library(data.table)
#ETV1=fread("./Tadarida/rounds/compli_validation190220/ETVtot.csv")
#ETV2=fread("./Tadarida/rounds/compli_validation190220/EXHAUSTIFS_TOTAL_ded.csv")
DataRP=fread("DataRP.csv")
IdExp=fread("./mnhn/CHIRO_EXPANSION_2014.csv")
VCcorr=fread("./mnhn/VCcorr.csv")
SpeciesList=fread("SpeciesList.csv")
IdExp$ESPECE[IdExp$ESPECE=="Hypsugo_savi"]="Hypsugo savii"
IdExp$ESPECE[IdExp$ESPECE=="Miniopterus_schreibersi"]="Miniopterus_schreibersii"
IdExp$ESPECE[IdExp$ESPECE=="Myotis_brantii"]="Myotis_brandtii"
IdExp$ESPECE[IdExp$ESPECE=="Myotis_daubentoni"]="Myotis_daubentonii"
IdExp$ESPECE[IdExp$ESPECE=="Myotis_puniclus"]="Myotis_punicus"
IdExp$ESPECE[IdExp$ESPECE=="nyctalus_noctula"]="Nyctalus_noctula"
IdExp$ESPECE[IdExp$ESPECE=="Pipistrellus_kulhii"]="Pipistrellus_kuhlii"
IdExp$ESPECE[IdExp$ESPECE=="Pipistrellus_nathusius"]="Pipistrellus_nathusii"
IdExp$ESPECE[IdExp$ESPECE=="pipistrellus_pipistrellus"]="Pipistrellus_pipistrellus"
IdExp$ESPECE[IdExp$ESPECE=="Plecotus_auricus"]="Plecotus_auritus"
IdExp$ESPECE[IdExp$ESPECE=="Plecotus_macrobularis"]="Plecotus_macrobullaris"
IdExp$ESPECE[IdExp$ESPECE=="Vesprtilio_murinus"]="Vespertilio_murinus"


IdExp$SciName=gsub("_"," ",IdExp$ESPECE)
test=match(IdExp$SciName,SpeciesList$`Scientific name`)
table(subset(IdExp$ESPECE,is.na(test)))

for (i in 1:unique(IdExp$ID_CIRCUIT))
{
  Idi=subset(IdExp,IdExp$ID_CIRCUIT==unique(IdExp$ID_CIRCUIT)[i])
  NumNew=VCcorr$NewID[VCcorr$ID_CIRCUIT==unique(IdExp$ID_CIRCUIT)[i]]
  if(is.na(NumNew)){NumNew=unique(IdExp$ID_CIRCUIT)[i]}
  if(as.numeric(NumNew)>1000){NumNew=-as.numeric(NumNew)}
  Datai=subset(DataRP,DataRP$num_site==NumNew)
  for (j in 1:unique(Idi$ANNEE))
  {
    Idj=subset(Idi,Idi$ANNEE==unique(Idi$ANNEE)[j])
    Dataj=subset(Datai,substr(Datai$date_debut,7,10)==unique(Idi$ANNEE)[j])
    for (k in 1:unique(Idi$PASSAGE))
    {
      Idk=subset(Idj,Idj$PASSAGE==unique(Idj$PASSAGE)[k])
      Datak=subset(Dataj,Dataj$passage==unique(Idj$PASSAGE)[k])
      Tronk=tstrsplit(Idk$ID_TRONCON_SESSION,split="Tron")[[2]]
      for (l in 1:unique(Tronk))
      {
        Idl=subset(Idk,Tronk==unique(Tronk)[l])
        Datal=subset(Datak,Datak$Tron==unique(Tronk)[l])
        for (m in 1:nrow(Idl))
        {
          Idl$SciName[m]
          Idl$TEMPS
        }
      }
    }
  }
  
  
}

