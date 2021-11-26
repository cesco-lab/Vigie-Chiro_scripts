library(data.table)
PredictAct="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc/Predict_Act.r"
PredictDM="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc/Predict_DM.r"
PredictPresence="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc/Predict_Presence.r"

Interpole="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc/Interpole.r"

args=vector()
args[4]="SpeciesList.csv"
#args[4]="SpeciesList_bird2018-12-21.csv"
#args[13]=c("TURPHI")
args[2]="GI_RandPts_France_dep_L93Radius_ 11000_1000"
#args[2]="GI_SysGrid__11_30_34_48_66_15000"
args[2]="GI_SysGrid__France_Spain_Switzerland_Italy_-2e+06_3e+06_4e+06_8e+06_1000"
args[3]="01/08/2019" #date of prediction
#ListPattern=90 #Threshold
ListPattern="xportTot" #Threshold
#ListPattern="_filtree" #Threshold
#ListPattern=NA #Threshold
#ListPattern=list(c("stacinus","hsteinii","alcathoe","brandtii","rginatus"
#                  ,"aterreri")) #Suffix
#ListPattern=list(c(" murinus","ilssonii"," noctula","leisleri","erotinus")) #Suffix
#ListPattern=list(c(" blythii","s myotis"," punicus")) #Suffix
#ListPattern=list(c("paccinii","bentonii")) #Suffix
#ListPattern=list(c("nathusii","s kuhlii"))
#ListPattern=list(c("osi"_DSnorm.learner"deros"," euryale"))
#ListPattern=list(c("striacus"," auritus","bullaris"))
ListPattern=list(c("_DS2.learner"))

args[7]="France_dep_L93Radius_ 11000.shp"
#args[7]="France_dep_L93_2A_2B"
args[7]="World_L93_France_Spain_Switzerland_Italy_-2e+06_3e+06_4e+06_8e+06.shp"
#args[7]="World_L93Radius_ 260000.shp"
args[8]=10000 #raster pixel size?
args[9]=F #DM
args[10]=F #Act
args[12]=T #Presence
args[11]=40 #number of coordinates projections (must be a division of 360)
GroupFilter="bat"
GroupFilter=NA
#PropGroup="Plecos"
PropGroup=NA

Prefix="./VigieChiro/ModPred/ModRF"


#if(!is.na(args[4]))
#{
SpeciesList=fread(args[4])
#}else{
#  Species=args[13]
#  SpeciesList=data.frame(Esp=Species)
#}
ListSp=SpeciesList$Esp


if(!is.na(GroupFilter))
{
  ListSp=subset(ListSp,SpeciesList$Group %in% GroupFilter)
  SpeciesList=subset(SpeciesList,SpeciesList$Group %in% GroupFilter)
}


#ListSp="thusii"
#ListSp=c("Myotis","Plecos","PipKN","NlTt","MyoAqu"
#,"RhiEH")
#ListSp="NlTt"

if(is.list(ListPattern))
{
  ListSp=substr(unlist(ListPattern),3,8)
}

for (h in 1:length(ListSp))
{
  
  Pattern=unlist(ListPattern)[h]
  Suffix=Pattern
  ModRF_fileList=list.files(dirname(Prefix),pattern=Pattern,full.names=T)  
  
  for (i in 1:length(ModRF_fileList))
  {
    ModRF_file=ModRF_fileList[i]
    args[1]=gsub("ModRFPresence_","",basename(ModRF_file))
    args[1]=gsub(".learner","",args[1])
    args[1]=gsub(" ","_",args[1])
    
    
    source(PredictPresence)
    print(ModRF_file)
    args[6]=FilName
    Sys.time()
    source(Interpole)
    Sys.time()
  }
}

