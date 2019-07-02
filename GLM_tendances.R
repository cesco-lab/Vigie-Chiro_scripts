GLMpackage=list.files("./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/functions/GLMs"
                      ,full.names=T)
ListDataSp=list.files("./VigieChiro/DataSp/RPCirw0_90",full.names=T)

TagData="Seuil90"
TagModel="GLM_tendancesfacteur_"
VI="nb_contacts_strict"
LE=c("year","poly(julian,2)","sample_cat","nb_Tron_strict"
     ,"temps_enr_strict","latitude","longitude","expansion_direct"
)
AF="year"

for (i in 1:length(GLMpackage))
{
  source(GLMpackage[i])
}

for (i in 1:length(ListDataSp))
{
  print(ListDataSp[i])
  Sp_GLM_short(
    dataFile=ListDataSp[i]
    ,
    varInterest=VI
    ,
    listEffects=LE
    ,
    selSample=1e10
    ,
    interactions=NA
    ,
    formulaRandom="+(1|site)"
    ,
    tagModel=paste0(TagModel
                    ,gsub(".csv","",basename(ListDataSp[i])),"_",TagData)
    ,
    family="nbinom2"
    ,
    asfactor=AF
  )
  
  
}
