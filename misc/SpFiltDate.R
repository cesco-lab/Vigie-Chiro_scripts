library(data.table)
FSpNuit="SpNuit2_50_DataLP_PF_exportTot.csv"
SpNuit=fread(FSpNuit)
DateFilt=as.Date("15/04/2019")
Particip=fread("C:/wamp64/www/p_export.csv")

ParticipFilt=subset(Particip,Particip$trait_debut>DateFilt)

SpNuitFilt=subset(SpNuit,SpNuit$participation %in% ParticipFilt$participation)

fwrite(SpNuitFilt,paste0(tools::file_path_sans_ext(FSpNuit),"_Filt"
                         ,DateFilt,".csv"))
