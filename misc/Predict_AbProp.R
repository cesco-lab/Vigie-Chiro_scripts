library(data.table)
Combinaisons=fread("PourAbProp.csv")
#Interpole="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Interpole.r"
args=vector()
args[7]="France_dep_L93_09_11_12_30_31_32_34_46_48_65_66_81_82_2A_2B_13_83_84_04_05_06_07_26_69_42_38_73_74_01_03_63_43_15_19_23_87_86_79_17_16_33_40_47_64_24.shp"
#args[7]="France_dep_L93.shp"
args[8]=20000 #PixelSize
#ModRF_file=paste0("./VigieChiro/ModPred/ModRFActLog_",args[1],"_Seuil",args[5],".learner")
#load(ModRF_file)


for (i in 1:nrow(Combinaisons))
{
  
FAbondance=Combinaisons$Fabondance[i]
FProportion=Combinaisons$Fproportion[i]
print(Sys.time())
print(basename(FProportion))
Abondance=fread(FAbondance)
Proportion=fread(FProportion)
#plot(Abondance$Group.2,Proportion$Group.2)
AbProp=Abondance
AbProp$pred=Abondance$pred*Proportion$pred
AbProp$err=NULL
NameAbProp=gsub("Presence","AbProp",FProportion)
fwrite(AbProp,NameAbProp)

args[6]=gsub(".csv","",NameAbProp)

source(Interpole)
}

