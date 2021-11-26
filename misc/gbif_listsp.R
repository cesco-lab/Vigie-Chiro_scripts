test=F


gbif_listsp=function(countrylist,group,rank){
  library(rgbif)
  library(data.table)
  CountryList=countrylist
  
  GroupName=group
  Rank=rank
  #Group=name_lookup(GroupName,rank=Rank,return="data",limit=99999)
  Group=name_backbone(name=GroupName,rank=Rank)
  #Group$key
  
  
  CountryFilter=vector()
  for (i in 1:length(CountryList))
  {
    temp_code <- isocodes[grep(CountryList[i], isocodes$name), "code"]
    CountryFilter=c(CountryFilter,temp_code)
  }
  
  
  #name_lookup(rank="SPECIES",higherTaxonKey = 137356451,country=france_code)
  #test=name_lookup("Bos gruniens",rank="SPECIES")
  
  #NOcc=rep(0,length(Group))
  #NOcc=vector()
  #for (h in 1:length(CountryList))
  #{
  #for (i in 1:nrow(Group))
  #{
  # #NOcc[i]=NOcc[i]+occ_count(taxonKey=Group$key[i], country=CountryFilter[1])
  #NOcc=c(NOcc,occ_count(taxonKey=Group$key[i], country=CountryFilter[1]))
  #if(i%%1000==1){print(paste(i,nrow(Group),Sys.time()))}
  #}
  
  #}
  #plot(NOcc)
  #NumSel=which.max(NOcc)
  
  Kids=name_lookup(higherTaxonKey = Group$usageKey,return='data',limit=99999
                   ,status='ACCEPTED')
  if(sum(grepl("rank",names(Kids)))>0)
  {
    Kids_sp=subset(Kids,Kids$rank=="SPECIES")
  }else{
    Kids_sp=Kids
  }
  print(GroupName)
  print("number of species:")
  print(nrow(Kids_sp))
  
  Ntot=vector()
  country=vector()
  species=vector()
  for (h in 1:length(CountryFilter))
  {
    for (i in 1:nrow(Kids_sp)) #1000 species per minute
    {
      Ndata=occ_count(taxonKey = Kids_sp$key[i]
                      , country=CountryFilter[h]
                      #,georeferenced=T
      )
      Ntot=c(Ntot,Ndata)
      country=c(country,CountryFilter[h])
      species=c(species,Kids_sp$canonicalName[i])
      if(i%%1000==1)
      {
        print(paste(CountryFilter[h],Sys.time(),i))
        NbSp=aggregate(Ntot,by=list(species),FUN=sum)
        print(paste("Nb species:",length(subset(NbSp$x,NbSp$x>0))))
        print(paste("Nb record:",sum(Ntot)))
      }
      
    }
  }
  print(max(Ntot))
  ListSp=data.frame(species,country,Ntot)
  ListSpPos=subset(ListSp,ListSp$Ntot>0)
  
  Suffix=""
  for (i in 1:length(CountryFilter))
  {
    Suffix=paste(Suffix,CountryFilter[i],sep="_")
  }
  
  fwrite(ListSpPos,paste0("./VigieChiro/gbifData/ListSp/ListSp_",GroupName,Suffix,".csv"))
  if(nrow(ListSpPos)>0)
  {
    SpCountry=aggregate(ListSpPos$species
                        ,by=list(ListSpPos$country)
                        ,FUN=length)
    barplot(SpCountry$x,names.arg=SpCountry$Group.1,main=group)
  }
}

if(test)
{
  gbif_listsp(countrylist=c("France","Spain","Italy","Switzerland")
              ,group="ASTERACEAE"
              ,rank="FAMILY")
}