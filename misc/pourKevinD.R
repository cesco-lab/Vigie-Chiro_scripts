library(data.table)


DataOpen=fread("C:/Users/ybas/Downloads/DataOpeni.csv")
GI=fread("C:/Users/ybas/Downloads/GI_FR_sites_localites.csv")
#names(GI)
GI$PropForet=GI$SpHO16S+GI$SpHO17S
GI$Foret=(GI$PropForet>0.8)
GI_foret=subset(GI,GI$Foret)
DataForet=subset(DataOpen
                 ,(paste(DataOpen$longitude,DataOpen$latitude) %in%
                     paste(GI_foret$X,GI_foret$Y)))
DataForet=subset(DataForet,DataForet$nuit_complete)
PrefPar=substr(DataForet$participation,1,3)
PrefParU=unique(PrefPar)
PrefParU=sample(PrefParU)
DataSel=DataOpen[0,]
Nsites=0
r=1
while(Nsites<100){
  print(PrefParU[r])
  Datar=subset(DataForet,PrefPar==PrefParU[r])
  Nsr=length(unique(Datar$numero_carre))
  Nsites=Nsites+Nsr
  print(Nsites)
  r=r+1
  if(r>length(PrefParU)){stop("PrefParU too short")}
}
PrefSel=PrefParU[1:(r-1)]
PrefSel=PrefSel[order(PrefSel)]
PrefSel
