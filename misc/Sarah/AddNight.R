library(data.table)

#AllDataFile=fread("C:/Users/yvesb/Downloads/final_data_update.csv")
DurSeqDir="C:/Users/yvesb/Downloads/Durseq"
DirOut="C:/Users/yvesb/Downloads/DurseqNightSplit"

dir.create(DirOut)

DurSeqFiles=list.files(DurSeqDir,full.names=T,pattern="duree_seq")


for (i in 1:length(DurSeqFiles)){
  DurSeqi=fread(DurSeqFiles[i])
  DurSeqi$nuit=as.Date(DurSeqi$TempsDebutSequence-12*3600)
  fwrite(DurSeqi,gsub(DurSeqDir,DirOut,DurSeqFiles[i]),sep=";")
}
