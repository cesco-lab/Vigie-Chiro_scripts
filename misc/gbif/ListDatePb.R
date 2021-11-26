library(data.table)
ListDate=list.files("./VigieChiro/gbifData/DateSp",full.names=T)
ListList=list.files("./VigieChiro/gbifData/ListSp",full.names=T,pattern=".csv$")

LDG=tstrsplit(basename(ListDate),split="_")[[2]]
LDG=gsub(".csv","",LDG)
LLG=tstrsplit(basename(ListList),split="_")[[2]]
DateMissing=subset(LLG,!(LLG %in% LDG))


for (i in 31:length(ListDate))
{
  Di=fread(ListDate[i])
  LLi=subset(ListList,grepl(paste0("_",LDG[i]),basename(ListList)))
  if(length(LLi)==1)
  {
    Li=fread(LLi[1])
    Dinew=subset(Di,Di$ListSpValide %in% Li$species)
    if(nrow(Dinew)<nrow(Di))
    {
      print(paste(LDG[i]," : ",nrow(Di)-nrow(Dinew)," espèces retirées"))
    }
    fwrite(Dinew,ListDate[i],sep=";")
  }else{
    stop("problem files")
  }
  
}
