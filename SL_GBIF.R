library(data.table)
library(rgbif)
#library(raster)
#library(png)
SpeciesList=fread("SpeciesList.csv")

Groupfilter=c("bush-cricket")


#res <- occ_download('taxonKey = 104912066', 'hasCoordinate = TRUE',user="ybas"
 #                   ,pwd="blip1705",email="yves.bas@mnhn.fr")
#occ_download_meta(res)

OccSL=data.frame()

for (i in 1:nrow(SpeciesList))
{
Sys.time()
  if((SpeciesList$Group[i] %in% Groupfilter)&(!(grepl("sp.",SpeciesList$`Scientific name`[i]))))
  {
    print(SpeciesList$Esp[i])
  key <- name_suggest(q=SpeciesList$`Scientific name`[i], rank='species')$key[1]

#  x <- map_fetch(taxonKey = key, year = 2000:2017,format=".mvt") # can be useful to gain time but less accurate in climate (especially in mountain regions)
  # spplot(x)
  
  
  
    Sys.time()
  Otemp=occ_search(taxonKey=key, limit=200000,return='data'
                   ,fields=c('decimalLatitude','decimalLongitude'))

  if(length(Otemp)>0){
    
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

Suffix=""

if(exists("Groupfilter")){
  if(length(Groupfilter)>0)
  {
    for (j in 1:length(Groupfilter))
    {
    Suffix=paste(Suffix,Groupfilter[j],sep="_")
    }
  }
}

  
fwrite(OccSL,paste0("./vigiechiro/Traits/GBIF/OccSL",Suffix,".csv"))
