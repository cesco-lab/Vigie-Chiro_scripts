library(data.table)
ScriptFolder="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc"
PredictAct="/Predict_Act.r"
PredictDM="/Predict_DM.r"
PredictPresence="/Predict_Presence.r"
Interpole="/Interpole.r"

args=vector()
args[4]="SpeciesList.csv"
#args[4]=NA
#args[4]="SpeciesList_bird2018-12-21.csv"
#args[13]=c("TURPHI")
args[2]="GI_SysGrid__20000"
#args[2]="GI_SysGrid_Radius_ 90000_50000"
#args[2]="GI_SysGrid__France_Spain_Switzerland_Italy_-2e+06_3e+06_4e+06_8e+06_1000"
args[3]="15/07/2019" #date of prediction
args[3]="All" #date of prediction
ListPattern=50 #Threshold
ListPattern="xportTot" #Threshold
#ListPattern="_filtree" #Threshold
#ListPattern="_DS2" #Threshold
#ListPattern=list(c("stacinus","hsteinii","alcathoe","brandtii","rginatus"
 #                  ,"aterreri")) #Suffix
#ListPattern=list(c(" murinus","ilssonii"," noctula","leisleri","erotinus")) #Suffix
#ListPattern=list(c(" blythii","s myotis"," punicus")) #Suffix
#ListPattern=list(c("paccinii","bentonii")) #Suffix
#ListPattern=list(c("nathusii","s kuhlii"))
#ListPattern=list(c("osideros"," euryale"))
#ListPattern=list(c("striacus"," auritus","bullaris"))
#ListPattern=list(c("R4___130"))

#args[7]="France_dep_L93Radius_ 11000.shp"
#args[7]="France_dep_L93_2A_2B"
args[7]="France_dep_L93.shp"
#args[7]="World_L93_France_Spain_Switzerland_Italy_-2e+06_3e+06_4e+06_8e+06.shp"
#args[7]="World_L93Radius_ 260000.shp"
args[8]=3000 #raster pixel size?
args[9]=F #DM
args[10]=T #Act
args[12]=F #Presence
args[11]=40 #number of coordinates projections (must be a division of 360)
#GroupFilter="bat"
GroupFilter=NA
#PropGroup="Plecos"
PropGroup=NA
Raster=F

Prefix="./VigieChiro/ModPred/ModRF"
#Prefix="ModRF"

if(!is.na(args[4]))
{
SpeciesList=fread(args[4])
}else{
  ListMod=list.files(dirname(Prefix),pattern=paste0(ListPattern[1],".learner"))
  Species=paste(tstrsplit(ListMod,split="_")[[2]]
                ,tstrsplit(ListMod,split="_")[[3]],sep="_")
  SpeciesList=data.frame(Esp=Species)
}
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

if(args[3]=="All")
{
  Month=c(1:12)
  Day=c(1,15)
}else{
  Month=substr(args[3],4,5)
  Day=substr(args[3],1,2)
}


for (f in Month)
{
print(f)
    for (g in Day)
  {
print(g)
      args[3]=paste(g,f,"2020",sep="/")
      
for (h in 1:length(ListSp))
{
  if(is.list(ListPattern))
  {
    Pattern=unlist(ListPattern)[h]
  }else{
    Pattern=ListPattern
  }
  
  if(!is.na(Pattern))
  {
    
      Suffix=paste0(Pattern,".learner")  
    }else{
    Suffix=paste0("NA.learner")  
    
  }
  
  
  args[1]=ListSp[h]
  ModRF_file=paste0(Prefix,"DecMin_",ListSp[h],Suffix)  
  
  if(file.exists(ModRF_file))
  {
    
    if(args[9]==T)
    {
      if (SpeciesList$Group[h]=="bat")
      {
        source(paste0(ScriptFolder,PredictDM))
        print(ListSp[h])
        args[6]=FilName
        ModRF=ModRF_DM
        source(paste0(ScriptFolder,Interpole))
      }
    }
  }
  
  ModRF_file=paste0(Prefix,"ActLog_",ListSp[h],Suffix)  
  
  if(file.exists(ModRF_file))
  {
    
    
    if(args[10])
    {
      print(Sys.time())
      source(paste0(ScriptFolder,PredictAct))
      print(ListSp[h])
      args[6]=FilName
      if(Raster){
      source(paste0(ScriptFolder,Interpole))
      }
        print(Sys.time())
    }
  }else{
    if(is.na(PropGroup))
    {
      ModRF_file=paste0(Prefix,"Presence_",ListSp[h],Suffix)  
      
    }else{
      ModRF_file=paste0(Prefix,"Presence_",PropGroup,Suffix)  
      
    }
    
      if(file.exists(ModRF_file))
    {
      if(args[12])
      {
        source(paste0(ScriptFolder,PredictPresence))
        print(as.character(ListSp[h]))
        args[6]=FilName
        source(paste0(ScriptFolder,Interpole))
      }
    }
  }
}


#Importance=as.vector(scale(ModRF$importance[,1])+scale(ModRF$importance[,2]))
#Imp2=as.data.frame(cbind(Var=row.names(ModRF$importance)
#                         ,Imp=Importance))
#head(Imp2[order(as.numeric(as.character(Imp2$Imp)),decreasing=T),],20)
