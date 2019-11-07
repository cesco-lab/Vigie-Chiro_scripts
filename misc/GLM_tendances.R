GLMpackage=list.files("./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/functions/GLMs"
                      ,full.names=T)
ListDataSp=list.files("./VigieChiro/DataSp/RPCirw0_50_Direct",full.names=T)

TagData="Seuil50"
TagModel="GLM_tendancesfacteur_flexibledirect"
VI="nb_contacts_flexible"
LE=c("year","sample_cat","julian","I(julian^2)"
     ,"temps_enr_flexible","latitude","I(latitude^2)"
     ,"longitude","I(longitude^2)"
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
