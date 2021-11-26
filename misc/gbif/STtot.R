library(data.table)

STselist=list.files("./VigieChiro/gbifData/ST/STsel",full.names=T)

STlist=list()
for (i in 1:length(STselist))
{
  STlist[[i]]=fread(STselist[i])
  Day=gsub("STseltot_","",basename(STselist[i]))
Day=gsub(".csv","",Day)
STlist[[i]]$Day=Day
}
STtot=rbindlist(STlist)
STtot=cbind(STtot$Day,STtot)
fwrite(STtot,"STtot.csv",sep=";")
