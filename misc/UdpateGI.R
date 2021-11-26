library(data.table)
FNewGI="./VigieChiro/GIS/GI_coordWGS84_2019-11-26.csv"
FOldGI="./VigieChiro/GIS/GI_coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv"

NewGI=fread(FNewGI)
OldGI=fread(FOldGI)

test=match(names(OldGI),names(NewGI))
Missing=subset(names(OldGI),!(names(OldGI) %in% names(NewGI)))

GItot=rbindlist(list(OldGI,NewGI),use.names = T,fill=T)

GItot[is.na(GItot)]=0

print(nrow(unique(cbind(GItot$Group.1,GItot$Group.2))))
print(nrow(GItot))

fwrite(GItot,FOldGI,sep=";")
