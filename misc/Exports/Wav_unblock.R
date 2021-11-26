DirB="C:/Users/yvesb/Documents/PI_autres/par/60f92d0f585d01000f012ce3"

SDirB=list.dirs(DirB)

SDirBlock=subset(SDirB,grepl("bloc",SDirB))

for (i in 1:length(SDirBlock))
{
  LWi=list.files(SDirBlock[i],full.names=T)
  Remisei=paste0(dirname(SDirBlock[i]),"/",basename(LWi))
  file.rename(from=LWi,to=Remisei)
}
