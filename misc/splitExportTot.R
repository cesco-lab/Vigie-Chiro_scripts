library(data.table)
ET=fread("D:/exporttot.txt")
Pref=substr(ET$participation,1,3)
table(Pref)
Prefu=unique(Pref)
for (i in 1:length(Prefu))
{
  print(paste(Prefu[i],Sys.time()))
  EP=subset(ET,Pref==Prefu[i])
  fwrite(EP,paste0("D:/export_",Prefu[i],".csv"),sep=";")
}
