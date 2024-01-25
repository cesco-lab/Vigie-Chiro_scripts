library(data.table)

DateDir="C:/Users/ybas/Documents/VigieChiro/gbifData/Dates"
DateAllDir="C:/Users/ybas/Documents/VigieChiro/gbifData"

ListDate=list.files(DateDir,full.names=T)

DateList=list()
for (z in 1:length(ListDate)){
  DateList[[z]]=fread(ListDate[z])
  Zone=gsub("_simplified.csv","",basename(ListDate[z]))
  Zone=gsub("DateSp_","",Zone)
  DateList[[z]]$zone=Zone
}
DateAll=rbindlist(DateList)

ListSp=unique(DateAll$ListSpValide)
DateSel=DateAll[0,]
for (a in 1:length(ListSp)){
  
  Datea=subset(DateAll,DateAll$ListSpValide==ListSp[a])
  DateaSel=subset(Datea,Datea$N==max(Datea$N))[1,]
  DateSel=rbind(DateSel,DateaSel)
  if(a%%1000==1){print(paste(a,ListSp[a],DateaSel$zone,DateaSel$PicSp))}
}

DateAll=subset(DateAll,!grepl(" x ",DateAll$ListSpValide))
DateSel=subset(DateSel,!grepl(" x ",DateSel$ListSpValide))
DateSel$Order=sample.int(nrow(DateSel))
#plot(DateSel$Order)

#hist(DateSel$PicSp,breaks=25)
fwrite(DateAll,paste0(DateAllDir,"/DateAll.csv"),sep=";")
fwrite(DateSel,paste0(DateAllDir,"/DateSel.csv"),sep=";")


