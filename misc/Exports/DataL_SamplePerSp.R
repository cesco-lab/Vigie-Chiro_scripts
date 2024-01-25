library(data.table)

DataL=fread("C:/Users/yvesb/Downloads/DataL2021-12-14.csv")
SpeciesList=fread("C:/Users/yvesb/Documents/SpeciesList.csv")
Nfiles=120
#SelSp=c("Pipkuh","Pipnat","Rhihip","Rhieur","Nyclas","Tadten"
 #       ,"Pippip","Pippyg","Minsch","Barbar","Eptser","Nyclei","Nycnoc"
  #      ,"Myodau","Myomys","Myonat","MyoGT","Pleaus","Pleaur","Plemac"
   #   ,"Hypsav","Rhifer","Vesmur","Eptnil","Myoalc","Myobec","Myoema")
SelSp=NA

BatList=subset(SpeciesList,SpeciesList$Group=="bat")

DataBat=subset(DataL,DataL$espece %in% BatList$Esp)
names(DataBat)[10]="temps_fin"
DataBat$duree=DataBat$temps_fin-DataBat$temps_debut

if(!is.na(SelSp[1])){
  DataBat=subset(DataBat,DataBat$espece %in% SelSp)
}

ListSp=unique(DataBat$espece)

DataSample=DataBat[0,]
for (i in 1:Nfiles)
{
  ProbMin=sample(c(1:1000)/1000,1)
  DureeMin=sample(c(1:50)/5,1)
  SpSel=sample(ListSp,1)
  DataSel=subset(DataBat,(DataBat$espece==SpSel[1])&(DataBat$probabilite>ProbMin)&(DataBat$duree>DureeMin))
  DataS1=DataSel[sample(nrow(DataSel),1),]
  DataSample=rbind(DataSample,DataS1)
  }
table(DataSample$espece)

fwrite(DataSample,paste0("DataSample",Sys.Date(),".csv"),sep=";")
