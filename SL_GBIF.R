library(data.table)
library(rgbif)
#library(raster)
#library(png)
SpeciesList=fread("SpeciesList.csv")


SpNameFilter=c("Gagea polidorii","Gagea dubia","Gagea erubescens"
               ,"Gagea foliosa","Gagea granatelli","Gagea luberonensis"
               ,"Gagea minima")
Groupfilter=c("plant")
CountryFilter="FR"

test=match(SpeciesList$`Scientific name`,SpNameFilter)
SpeciesList=subset(SpeciesList,!is.na(test))


#res <- occ_download('taxonKey = 104912066', 'hasCoordinate = TRUE',user="ybas"
#                   ,pwd="blip1705",email="yves.bas@mnhn.fr")
#occ_download_meta(res)

OccSL=data.frame()

for (i in 1:nrow(SpeciesList))
{
  Sys.time()
  if((SpeciesList$Group[i] %in% Groupfilter)
     &(!(grepl("sp.",SpeciesList$`Scientific name`[i]))))
  {
    print(SpeciesList$Esp[i])
    print(Sys.time())
    key <- name_suggest(q=SpeciesList$`Scientific name`[i], rank='species')$key
    
    #  x <- map_fetch(taxonKey = key, year = 2000:2017,format=".mvt") # can be useful to gain time but less accurate in climate (especially in mountain regions)
    # spplot(x)
    Otemp=vector()
    j=1
    while(!is.data.frame(Otemp)){
      if(exists("CountryFilter"))
      {
        Otemp=occ_search(taxonKey=key[j], country=CountryFilter,limit=200000,return='data'
                         ,fields=c('decimalLatitude','decimalLongitude','individualCount','coordinateUncertaintyInMeters'))
      }else{
        Otemp=occ_search(taxonKey=key[j], limit=200000,return='data'
                         ,fields=c('decimalLatitude','decimalLongitude','individualCount','coordinateUncertaintyInMeters'))
      }
      j=j+1
      }
    
    
    if(length(Otemp)>1){
      Otemp=subset(Otemp,(Otemp$individualCount!=0)|(is.na(Otemp$individualCount)))
      if(nrow(Otemp)>0){
      print(plot(Otemp$decimalLongitude,Otemp$decimalLatitude,main=SpeciesList$Esp[i]))
      
        
      if(is.numeric(Otemp[[1]][1]))
      {
        print(paste(Sys.time(),nrow(Otemp)))
        Sys.time()
        Otemp$Esp=SpeciesList$Esp[i]
        OccSL=rbind(OccSL,Otemp)   
        #Otemp=occ_search(scientificName = SpeciesList$`Scientific name`[i])
      } 
      Sys.time()
      #hist(Otemp$decimalLatitude)
      #plot(Otemp$decimalLongitude,Otemp$decimalLatitude)
      
    }
  }
  }
}

Suffix=""

if(exists("Groupfilter")){
  if(length(Groupfilter)>0)
  {
    for (j in 1:length(Groupfilter))
    {
      Suffix=paste(Suffix,Groupfilter[j],sep="_")
    }
  }
}else{
  Suffix=paste(Suffix,substr(Sys.time(),1,10),sep="_")
}

if(exists("SpNamefilter")){
  for (i in 1:length(SpNamefilter))
  {
    Suffix=paste(Suffix,SpNameFilter[i],sep="_")
  }
}

fwrite(OccSL,paste0("./vigiechiro/Traits/GBIF/OccSL",Suffix,".csv"))
