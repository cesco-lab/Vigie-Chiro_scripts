library(data.table)

DirRaw="C:/Users/yvesb/Documents/VigieChiro/Raw/forGLM2203/"
#SRt=45
#SpeciesList=fread("C:/Users/yvesb/Documents/Tadarida/Tadarida-C/tadaridaC_src/other_inputs/SpeciesList.csv")
#SpeciesList=c("Barbar","Pippip","Pipkuh","Pipnat","Pippyg","Nyclei","Eptser","Nycnoc","Hypsav","Minsch"
 #             ,"Myospp","Plespp","Tadten","Nyclas","Rhihip","Rhifer")
Tmax=500
#ThresRP=20
#ThresPF=50
FOut=paste0("C:/Users/yvesb/Documents/mnhn/Mathilde/forGLM2203_Custom.csv")
CustomSorting=fread("SortingRPPFspecies.csv")

SpeciesList=CustomSorting$Species

FRaw=list.files(DirRaw,full.names=T)

dir.create(dirname(FOut))

DataAll=list()
for (i in 1:length(FRaw))
{
  print(FRaw[i])

  datai=fread(FRaw[i])
  print(nrow(datai))
  #print(nrow(datai))
  table(datai$protocole)
  Sp=subset(datai$espece,datai$espece!="")[1]
  if(Sp %in% SpeciesList){
  testS=match(Sp,CustomSorting$Species)
  dataS=subset(datai,datai$x>CustomSorting$SRt[testS])
  dataS=subset(dataS,dataS$temps_enr<Tmax)
  dataS=subset(dataS,!is.na(dataS$expansion_direct))
  dataS$score_max[is.na(dataS$score_max)]=1
  Sort=ifelse(dataS$protocole=="POINT_FIXE"
              ,dataS$score_max>CustomSorting$ThresPF[testS]/100
              ,dataS$score_max>CustomSorting$ThresRP[testS]/100)
  #table(Sort)
  dataS$nb_contacts=ifelse(Sort,dataS$nb_contacts,0)
  dataS$species=Sp
  dataS$ID=c(1:nrow(dataS))
  dataS$nb_contacts[is.na(dataS$nb_contacts)]=0
  #dataS$espece=Sp
  
  if(sum(is.na(dataS$longitude))>0){
    dataS$latitude=NULL
    dataS$longitude=NULL
  }
  
  table(dataS$protocole)
  
  head(dataS$site)
  dataS$cjulian=cos(dataS$julian/365*2*3.1416)
  dataS$sjulian=sin(dataS$julian/365*2*3.1416)
  dataS$cjulian2=dataS$cjulian^2
  dataS$sjulian2=dataS$sjulian^2
  #dataS=subset(dataS,dataS$species %in% SpeciesList)
  DataAll[[i]]=dataS
  }
  #print(nrow(dataS))
#fwrite(dataS,paste0(DirOut,"/",basename(FRaw[i])))
}
DataAllDF=rbindlist(DataAll)
fwrite(DataAllDF,FOut)
