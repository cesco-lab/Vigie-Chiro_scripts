library(data.table)
LT=fread("./output/LT_BonTron_log1.csv")
SpeciesList=fread("SpeciesList.csv")
Group="bat"
PriorityFields=c("espece","nom_espece","pourcentage_variation"
,"pourcentage_variation_sl","pourcentage_variation_ul","categorie_tendance_EBCC"
,"valide","raison_incertitude","p_value","TRaw"
)


GroupList=subset(SpeciesList$Esp,SpeciesList$Group==Group)

GroupT=subset(LT,LT$espece %in% GroupList)
GroupT$pourcentage_variation_sl=(GroupT$IC_inferieur)^
  (GroupT$derniere_annee-GroupT$premiere_annee)*100-100
GroupT$pourcentage_variation_ul=(GroupT$IC_superieur)^
  (GroupT$derniere_annee-GroupT$premiere_annee)*100-100
barplot(GroupT$pourcentage_variation_ul-GroupT$pourcentage_variation_sl
        ,names.arg=GroupT$espece,las=2,ylim=c(0,200))

OtherFields=subset(colnames(GroupT),!colnames(GroupT) %in% PriorityFields)
FieldOrder=c(PriorityFields,OtherFields)
GroupT=subset(GroupT,select=FieldOrder)

GroupT$pourcentage_variation=round(GroupT$pourcentage_variation)
GroupT$pourcentage_variation_sl=round(GroupT$pourcentage_variation_sl)
GroupT$pourcentage_variation_ul=round(GroupT$pourcentage_variation_ul)

GroupT=GroupT[order(GroupT$valide,decreasing = T),]

fwrite(GroupT,paste0("SynthesesTendances_",Group,".csv"),sep=";")

       