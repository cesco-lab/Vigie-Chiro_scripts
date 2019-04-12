Test=F
Presence_Absence=function(sptarget,sample)
{
  library(data.table)
  
  Occurences=fread(paste0("./VigieChiro/Traits/GBIF/OccSL_",sptarget,".csv"))
  SpTarget=sptarget
  #SpTarget="Scorus"
  Sample=sample
  
  
  OccPtot=subset(Occurences,Occurences$Esp==SpTarget)
  if(nrow(OccPtot)>1000)
  {
    OccP=subset(OccPtot,OccPtot$coordinateUncertaintyInMeters<100)
    if(nrow(OccP)<1000)
    {
      OccPsup=subset(OccPtot,is.na(OccPtot$coordinateUncertaintyInMeters))
      if(nrow(OccPsup)>Sample-nrow(OccP))
      {
        OccPsup=OccPsup[sample.int(nrow(OccPsup),Sample-nrow(OccP)),]
      }
      OccP=rbind(OccP,OccPsup)
      if(nrow(OccP)<1000)
      {
        OccPsup=subset(OccPtot,(OccPtot$coordinateUncertaintyInMeters<1000)
                       &(OccPtot$coordinateUncertaintyInMeters>=100))
        if(nrow(OccPsup)>Sample-nrow(OccP))
        {
          OccPsup=OccPsup[sample.int(nrow(OccPsup),Sample-nrow(OccP)),]
        }
        OccP=rbind(OccP,OccPsup)
        if(nrow(OccP)<1000)
        {
          OccPsup=subset(OccPtot,(OccPtot$coordinateUncertaintyInMeters>=1000))
          if(nrow(OccPsup)>Sample-nrow(OccP))
          {
            OccPsup=OccPsup[sample.int(nrow(OccPsup),Sample-nrow(OccP)),]
          }
          OccP=rbind(OccP,OccPsup)
        }
      }
    }else{
      OccP=OccP[sample.int(nrow(OccP),Sample),]
    }
  }else{
    OccP=OccPtot
  }
  
  OccAtot=subset(Occurences,Occurences$Esp!=SpTarget)
  if(nrow(OccAtot)>1000)
  {
    OccA=subset(OccAtot,OccAtot$coordinateUncertaintyInMeters<100)
    if(nrow(OccA)<1000)
    {
      OccAsup=subset(OccAtot,is.na(OccAtot$coordinateUncertaintyInMeters))
      if(nrow(OccAsup)>Sample-nrow(OccA))
      {
        OccAsup=OccAsup[sample.int(nrow(OccAsup),Sample-nrow(OccA)),]
      }
      OccA=rbind(OccA,OccAsup)
      if(nrow(OccA)<1000)
      {
        OccAsup=subset(OccAtot,(OccAtot$coordinateUncertaintyInMeters<1000)
                       &(OccAtot$coordinateUncertaintyInMeters>=100))
        if(nrow(OccAsup)>Sample-nrow(OccA))
        {
          OccAsup=OccAsup[sample.int(nrow(OccAsup),Sample-nrow(OccA)),]
        }
        OccA=rbind(OccA,OccAsup)
        if(nrow(OccA)<1000)
        {
          OccAsup=subset(OccAtot,(OccAtot$coordinateUncertaintyInMeters>=1000))
          if(nrow(OccAsup)>Sample-nrow(OccA))
          {
            OccAsup=OccAsup[sample.int(nrow(OccAsup),Sample-nrow(OccA)),]
          }
          OccA=rbind(OccA,OccAsup)
        }
      }
    }else{
      OccA=OccA[sample.int(nrow(OccA),Sample),]
    }
  }else{
    OccA=OccAtot
  }
  
  
  OccP$presence=1
  OccA$presence=0
  plot(OccA$decimalLongitude,OccA$decimalLatitude)
  points(OccP$decimalLongitude,OccP$decimalLatitude,col=2,pch=2)
  table(OccA$Esp)
  Occ=rbind(OccP,OccA)
  
  fwrite(Occ,paste0("./VigieChiro/GIS/PA_",SpTarget,".csv"))
  
}
if(Test)
{
  #for testing
  Presence_Absence(
    sptarget="Thymus nitens"
    ,sample=1000
  )
}